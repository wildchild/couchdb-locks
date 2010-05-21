require "spec_helper"

describe "POST to DB/_locks" do
  context "with scope" do
    it "should create a lock" do
      scope = "Person"
      response, json = create_lock(scope)
      response.code.should == 201
      json["ok"].should be_true
      json["id"].should be

      response = @resource["_locks"].get
      json = JSON.parse(response)
      json["rows"].should_not be_empty
      json["rows"][0]["scope"].should == scope
    end
  end

  context "without scope" do
    it "should create global lock" do
      response = @resource["_locks"].post(nil)
      response.code.should == 201
      json = JSON.parse(response)
      json["ok"].should be_true
      json["id"].should be

      response = @resource["_locks"].get
      json = JSON.parse(response)
      json["rows"].should_not be_empty
      json["rows"][0]["scope"].should == "global"
    end
  end

  context "with array of scopes" do
    it "should create multiple locks" do
      scopes = ["Scope1", "Scope2", "Scope3"]
      response, json = create_lock(scopes)
      response.code.should == 201
      json["ok"].should be_true
      json["id"].should be

      response = @resource["_locks"].get
      json = JSON.parse(response)
      json["total_rows"].should == 3
      json["rows"].map { |row| row["id"] }.compact.uniq.size.should == 1
      json["rows"].map { |row| row["scope"] }.should =~ scopes
    end
  end

  context "with already locked scope" do
    before(:all) do
      create_lock("MyScope")
    end

    it "should respond with 409 Conflict" do
      expect {
        create_lock("MyScope")
      }.to raise_error(RestClient::Conflict)
    end
  end

  context "with already locked scopes" do
    before(:all) do
      create_lock(["Scope1", "Scope2"])
    end

    it "should respond with 409 Conflict" do
      expect {
        create_lock(["Scope1", "Scope2", "Scope3"])
      }.to raise_error(RestClient::Conflict)
    end
  end
end

describe "GET to DB/_locks" do
  before(:all) do
    create_lock(%w(Scope1 Scope2 Scope3))
  end

  it "should list existing locks" do
    response = @resource["_locks"].get
    json = JSON.parse(response)
    json["total_rows"].should == 3
    json["rows"].size.should == 3
  end
end

describe "DELETE to DB/_locks" do
  before(:all) do
    %w(foo bar baz).each do |scope|
      create_lock(scope)
    end
  end

  it "should delete all locks" do
    response = @resource["_locks"].delete
    response.code.should == 200
    JSON.parse(response)["ok"].should be_true
  end
end

describe "DELETE to DB/_locks/Token" do
  before(:each) do
    response = @resource["_locks?scope=MyScope"].post(nil)
    @lock = JSON.parse(response)["id"]
  end

  context "with valid token" do
    it "should delete a lock" do
      response = @resource["_locks/#{@lock}"].delete
      response.code.should == 200
      JSON.parse(response)["ok"].should be_true
    end
  end
end

describe "DELETE to _all_locks" do
  before(:each) do
    %w(foo bar baz).each do |scope|
      create_lock(scope)
    end
  end

  it "should delete all locks" do
    response = @couchdb["_all_locks"].delete
    response.code.should == 200
    JSON.parse(response)["ok"].should be_true

    response = @resource["_locks"].get
    JSON.parse(response)["total_rows"].should == 0
  end
end

describe "PUT to _all_locks/_expire" do
  before(:each) do
    create_lock("foo", 0)
    create_lock("bar", 5000)
  end

  it "should expire locks" do
    response = @couchdb["_all_locks/_expire"].put(nil)
    response.code.should == 200
    json = JSON.parse(response)
    json["ok"].should be_true

    response = @resource["_locks"].get
    json = JSON.parse(response)
    json["total_rows"].should == 1
    json["rows"][0]["scope"].should == "bar"
  end
end

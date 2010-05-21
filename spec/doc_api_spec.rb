require "spec_helper"

describe "Document API request" do
  before(:each) do
    @doc = { "title" => "omg" }
    response, json = create_lock("omg")
    @lock = json["id"]
  end

  context "with POST verb" do
    it "should create new document" do
      response = @resource["_locks/_db?lock=#{@lock}"].post(@doc.to_json)
      JSON.parse(response)["ok"].should be_true
    end

    context "with invalid lock token" do
      it "should respond with 400 Bad Request" do
        expect {
          @resource["_locks/_db?lock=INVALID"].post(@doc.to_json)
        }.to raise_error(RestClient::BadRequest)
      end
    end
  end

  context "with PUT verb" do
    it "should create new document" do
      response = @resource["_locks/_db/omg?lock=#{@lock}"].put(@doc.to_json)
      JSON.parse(response)["ok"].should be_true
    end

    context "with invalid lock token" do
      it "should respond with 400 Bad Request" do
        expect {
          @resource["_locks/_db/omg?lock=INVALID"].post(@doc.to_json)
        }.to raise_error(RestClient::BadRequest)
      end
    end
  end

  context "with DELETE verb" do
    before(:each) do
      response = @resource.post(@doc.to_json)
      @json = JSON.parse(response)
    end

    it "should delete document" do
      response = @resource["_locks/_db/#{@json["id"]}?lock=#{@lock}&rev=#{@json["rev"]}"].delete
      JSON.parse(response)["ok"].should be_true
    end

    context "with invalid lock token" do
      it "should respond with 400 Bad Request" do
        expect {
          @resource["_locks/_db/#{@json["id"]}?lock=INVALID&rev=#{@json["rev"]}"].delete
        }.to raise_error(RestClient::BadRequest)
      end
    end
  end
end

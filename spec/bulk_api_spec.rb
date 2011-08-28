require "spec_helper"

describe "Bulk Document API request" do
  before(:each) do
    @docs = { "docs" => [{ "one" => 1}, { "two" => 2 }] }
    response, json = create_lock("omg")
    @lock = json["id"]
  end

  context "with POST verb" do
    it "should create new documents" do
      response = @resource["_locks/_db/_bulk_docs?lock=#{@lock}"].post(@docs.to_json, { "Content-Type" => "application/json" })
      json = JSON.parse(response)
      json.size.should == 2
    end

    context "with invalid lock token" do
      it "should respond with 400 Bad Request" do
        expect {
          @resource["_locks/_db/_bulk_docs?lock=INVALID"].post(@docs.to_json)
        }.to raise_error(RestClient::BadRequest)
      end
    end
  end
end

require "rubygems"
require "bundler"
Bundler.setup
Bundler.require

DB_NAME = "locks_test"
DB_HOST = "http://localhost:5984"

Rspec.configure do |config|
  config.before(:all) do
    @db_url = "#{DB_HOST}/#{DB_NAME}"
    @couchdb = RestClient::Resource.new(DB_HOST)
    @resource = RestClient::Resource.new(@db_url)
  end

  config.before(:each) do
    reset_database
  end

  config.after(:each) do
    reset_locks
  end

  def reset_database
    delete_database
    RestClient.put(@db_url, {})
  end

  def delete_database
    RestClient.delete(@db_url) rescue nil
  end

  def reset_locks
    @couchdb["_all_locks"].delete
  end

  def create_lock(scopes, timeout = nil)
    path = "_locks"
    path << "?timeout=#{timeout}" unless timeout.nil?
    response = @resource[path].post([scopes].flatten.to_json)
    return response, JSON.parse(response)
  end
end

@server_api = "https://api-rs-preprod.opscode.com"

# Set the Config
#
Chef::Config.http_retry_delay = 1
Chef::Config.http_retry_count = 2
Chef::Config.client_key = File.join(File.dirname(__FILE__), "key.pem")

# Go Go Go!
#
@orgs_and_clients.each do |org, client|
  Chef::Config.chef_server_url = "#{@server_api}/organizations/#{org}"
  Chef::Config.node_name = client

  puts "-- Beginning Organization #{org} --"

  # common object API
  #
  %w{roles environments}.each do |object|
    print "   * #{object}"
    objects = api.get(object)
    successes = 0
    objects.each do |name, url|
      begin
        api.get url
        successes += 1
      rescue
      end
    end
    puts " - #{successes}/#{objects.size}"
  end

  # cookbook API
  #
  print "   * cookbooks"
  cookbooks = api.get("cookbooks?num_versions=all")
  total_cookbooks = 0
  cookbook_successes = 0
  cookbooks.each do |cookbook_name, cookbook_data|
    cookbook_data["versions"].each do |version_data|
      total_cookbooks += 1
      begin
        # api.get(version_data["url"])
        cookbook_successes += 1
      rescue
      end
    end
  end
  puts " - #{cookbook_successes}/#{total_cookbooks}"

  # databag api
  #
  print "   * databags"
  databags = api.get("data")
  total_databag_items = 0
  databag_item_successes = 0
  databags.each do |databag_name, databag_url|
    databag_items = api.get(databag_url)
    databag_items.each do |databag_item_name, databag_item_url|
      total_databag_items += 1
      begin
        api.get(databag_item_url)
        databag_item_successes += 1
      rescue
      end
    end
  end
  puts " - #{databag_item_successes}/#{total_databag_items}"

  puts "-- End Organization #{org} --"
  puts
end

## Overview ##

The top-10-org validation script is intended to run on preprod.

## Setup ##

First, bundle install this bad boy (but if you have chef installed you should be good to go).

```
bundle install --binstubs
```    

Next you'll need to turn on authentication mocks in preprod with `oc_chef_replay`. From your `rs-preprod` repo:

```
knife ssh role:opscode-erchef 'sudo /srv/oc_chef_replay/oc_chef_replay erchef erchef rename'
```

## Usage ##

```
./bin/knife exec validate-top-10.rb
```

## Cleanup ##

You should restore the original authentication code in preprod erchef when you are finished:

```
knife ssh role:opscode-erchef 'sudo /srv/oc_chef_replay/oc_chef_replay erchef erchef restore'
```

## Details ##

The organizations and the IDs associated with them were obtained from a Splunk search.
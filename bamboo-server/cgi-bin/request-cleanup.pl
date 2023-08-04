#!/usr/bin/perl

# Authors: Darin Nikolow
# Copyright (C) 2022 ACK CYFRONET AGH
# This software is released under the MIT license cited in 'LICENSE.txt'

# This CGI script is triggered by grafana (web-hook) when storage gets low.
# The script processes the json data sent via POST HTTP method to get
# a list of agents to cleanup.
# The script resides on the bamboo server in /usr/lib/cgi-bin.
# Note that a running apache http on port 3080 is required. 
# The apache server should allow CGI requests.
#

use strict;
use warnings;
use Data::Dumper;   
use CGI;
use JSON;

select(STDERR);
$| = 1;
select(STDOUT);
$| = 1;

my $q = CGI->new;

# Process an HTTP request
my $value   = $q->param('param_name');

# Prepare various HTTP responses
print $q->header();
#print $q->header('application/json');

open(my $out, ">>",  "/tmp/output.txt") or die "Can't open output.txt: $!";
print $out scalar $q->param('POSTDATA');
my $dj = decode_json($q->param('POSTDATA'));
print $out "\nDecoded json:", Dumper $dj, "\n";
print $out "state: ", $dj->{'state'}, "\n";
print $out "first agent: ", $dj->{'evalMatches'}[0]{'metric'};
print $out "size of evalMatches: ", keys $dj->{'evalMatches'}, "\n";
if ($dj->{'state'} eq 'alerting') {
    print $out "Alerting...\n";
    foreach (keys $dj->{'evalMatches'}) {
	print $out "key: ", $_, "\n";
	my $agent = $dj->{'evalMatches'}[$_]{'metric'};
	system("sudo -u ubuntu /home/ubuntu/bin/cleanup-agent-rest.sh \'${agent}\' &");
    }
}

#!/usr/bin/env perl

# formalog-pengines

use KBS2::Util;
use UniLang::Util::TempAgent;

use Data::Dumper;

my $tempagent = UniLang::Util::TempAgent->new(Name => 'test-formalog-pengines-proxy');
my $res1 = $tempagent->MyAgent->QueryAgent
  (
   Receiver => 'Formalog-Pengines-Proxy',
   Data => {
	    Agent => 'Agent1',
	    Query => ['member',Var('?X'),['[|]','a','b']],
	   },
  );
print Dumper({Res1 => $res1});

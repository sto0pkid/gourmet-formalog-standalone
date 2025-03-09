#!/usr/bin/env perl

use Formalog::Client::Pengines;
use KBS2::Util;
use Data::Dumper;

$UNIVERSAL::debug = 1;

my $result1 = FormalogQuery
  (
   Agent => 'DMS-Formalog-Agent1',
   Vars => ['_prolog_list',Var('?Results')],
   Query => ['member',Var('?X'),['[|]','a','b']],
  );

print Dumper({DeleteResultFormalog => $result1});


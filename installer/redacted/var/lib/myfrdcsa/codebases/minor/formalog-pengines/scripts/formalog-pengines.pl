#!/usr/bin/env perl

use FormalogPengines::REPL;

use Data::Dumper;

my $repl = FormalogPengines::REPL->new();

while ($query = <>) {
  print Dumper($repl->DoQuery(Query => "formalogPenginesCall($query,MyResult)"));
}

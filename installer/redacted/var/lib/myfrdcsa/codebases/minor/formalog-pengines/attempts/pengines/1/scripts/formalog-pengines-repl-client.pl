#!/usr/bin/env perl

use lib "/var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1";

use FormalogPenginesREPL;

my $repl = FormalogPenginesREPL->new();

while ($query = <>) {
  $repl->DoQuery(Query => "formalogPenginesCall($query,MyResult)");
}

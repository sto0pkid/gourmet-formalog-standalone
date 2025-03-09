#!/usr/bin/env perl

use FormalogPengines::REPLNew;
use KBS2::Util qw(Var);

use Data::Dumper;

my $repl = FormalogPengines::REPLNew->new();

while ($query = <>) {
  print Dumper($repl->DoQuery
	       (
		Query => "formalogPenginesCall($query,MyResult)",
		Variables => Var('?X'),
	       ));
  print Dumper($repl->DoQuery
	       (
		Query => "formalogPenginesCall($query,MyResult)",
		Variables => ['_prolog_list',Var('?X')],
	       ));

}

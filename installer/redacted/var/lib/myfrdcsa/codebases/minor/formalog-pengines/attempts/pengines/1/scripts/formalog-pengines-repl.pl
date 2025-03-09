#!/usr/bin/env perl

use lib "/var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1";
# /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/ports.pl
# /var/lib/myfrdcsas/versions/myfrdcsa-1.0/sandbox-new/javapengine-20200506/javapengine-20200506/src/main/java/com/simularity/os/javapengine/example/ManualAsk.java
# cd /var/lib/myfrdcsa/sandbox/javapengine-20200506/javapengine-20200506 && ./frdcsa.sh
# cd /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/scripts/ && ./test.pl

# /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/FormalogPengines.pm

# $UNIVERSAL::debug = 1;

use FormalogPengines;
use KBS2::ImportExport;
use KBS2::Util qw(Var);

use Data::Dumper;
use JSON;

my $fp = FormalogPengines->new();
my $importexport = KBS2::ImportExport->new();

sub DoQuery {
  my (%args) = @_;
  print Dumper({Query => $args{Query}});
  print Dumper($fp) if $UNIVERSAL::debug;
  my $res = $fp->GetAll($args{Query});
  my $array = $res->toArray();
  print Dumper({Array => $array}) if $UNIVERSAL::debug;
  my @allresults;
  foreach my $elt (@$array) {
    print "<$elt>\n" if $UNIVERSAL::debug;
    push @allresults, ConvertJSONToPerl(Elt => $elt);
  }
  my $res1 = TranslateResultBack(Results => \@allresults);
  print Dumper({Res1 => $res1});
  return $res1;
}

sub TranslateResultBack {
  my (%args) = @_;
  print Dumper({Input => $args{Results}}) if $UNIVERSAL::debug;
  my @results;
  foreach my $r (@{$args{Results}}) {
    my $bindings = $r->{MyResult};
    # delete $r->{MyResult};
    my $vars = $bindings->[0];
    next unless $bindings->[1][0];
    my $res = $bindings->[1][0];
    my $i = 0;
    my $value = {};
    my $values = {};
    foreach my $var (@$vars) {
      $value->{$var} = $res->[$i++];
    }
    print Dumper({Value => $value}) if $UNIVERSAL::debug;
    foreach my $key (keys %$r) {
      if ($key ne 'MyResult') {
	print $key."\n" if $UNIVERSAL::debug;
	my $res1 = ConvertJavaPengineToInterlingua(Entry => $value->{$r->{$key}});
	if ($res1->{Success}) {
	  $values->{$key} = $res1->{Output};
	} else {
	  # FIXME: confused, throw an error
	}
      }
    }
    print Dumper({Value => $value,
		  Values => $values}) if $UNIVERSAL::debug;
    push @results, $values;
  }
  print Dumper({Response => \@results}) if $UNIVERSAL::debug;
  return \@results;
}

sub ConvertJSONToPerl {
  my (%args) = @_;
  print Dumper({Elt => $args{Elt}}) if $UNIVERSAL::debug;
  my $ds = from_json($args{Elt});
  # print Dumper($ds);
  return $ds;
}

sub DoQueryInterlingua {
  my (%args) = @_;
  my $input = $args{Input};
  my $res1 = $importexport->Convert
    (
     Input => $input,
     InputType => 'Interlingua',
     OutputType => 'Prolog',
    );
  print Dumper($res1);
  if ($res1->{Success}) {
    my $output = $res1->{Output};
    print "$output\n";
    my $query = "($output,transform_result(X,Y))";
    DoQuery(Query => $query);
  }
}

# DoQueryInterlingua(Input => [['member',Var('?X'),['_prolog_list',['a','taco',Var('?A'),Var('?_A'),'B',Var('?A'),'C D'],'b','dag',2,'2','c']]]);

# my $query = "member(X, [a(taco,A,_A,'B','C D'),b,dag,2,'2',c])";
# my $query = "(member(X,[a(taco,A,_A,'B',A,'C D'),b,dag,2,'2',c]),transform_result(X,Y))";

# DoQuery(Query => "formalogPenginesCall(member([Y,_C],[[a,_C],[c,d],[e,f]]),MyResult)");
# DoQuery(Query => "formalogPenginesCall(findall(X,member([Y,X],[[a,b],[c,d],[e,f]]),Xs),MyResult)");

while ($query = <>) {
  DoQuery(Query => "formalogPenginesCall($query,MyResult)");
}

# my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]),Result)";
# my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]))";

sub ConvertJavaPengineToInterlingua {
  my (%args) = @_;
  my @results;
  my $type = ref($args{Entry});
  if ($type eq 'ARRAY') {
    push @results, '_prolog_list';
    foreach my $subentry (@{$args{Entry}}) {
      my $res1 = ConvertJavaPengineToInterlingua(Entry => $subentry);
      if ($res1->{Success}) {
	push @results, $res1->{Output};
      }
    }
    return
      {
       Success => 1,
       Output => \@results,
      };
  } elsif ($type eq 'HASH') {
    if (exists $args{Entry}{functor}) {
      my $f = $args{Entry}{functor};
      if ($f eq '$NUMBER') {
	return
	  {
	   Success => 1,
	   Output => $args{Entry}{args}[0],
	  };
      } elsif ($f eq '$VARIABLE') {
	return
	  {
	   Success => 1,
	   Output => Var($args{Entry}{args}[0]),
	  };
      } else {
	# a regular predicate
	my $res1 = ConvertJavaPengineToInterlingua(Entry => $args{Entry}{args});
	if ($res1->{Success}) {
	  shift @{$res1->{Output}};
	  return
	    {
	     Success => 1,
	     Output => [$args{Entry}{functor},@{$res1->{Output}}],
	    };
	} else {
	  # FIXME: confused, throw an error
	}
      }
    } else {
      # FIXME: confused, throw an error
    }
  } elsif ($type eq '') {
    return
      {
       Success => 1,
       Output => $args{Entry},
      };
  }
}

package FormalogPengines::REPLNew;

use FormalogPengines;
use KBS2::Util qw(Var DeVar);
use PerlLib::SwissArmyKnife;

use Moose;

use Data::Dumper;
use JSON;

has MyFormalogPengines =>
  (
   is => "rw",
   isa => "FormalogPengines",
   default => sub {FormalogPengines->new()},
  );

sub DoQuery {
  my ($self,%args) = @_;
  if ($args{Debug}) {
    $UNIVERSAL::debug = $args{Debug};
  }
  print Dumper({Query => $args{Query}}) if $UNIVERSAL::debug;
  print Dumper({FormalogPengines => $self->MyFormalogPengines}) if $UNIVERSAL::debug;
  my $res = $self->MyFormalogPengines->GetAll($args{Query});
  my $array = $res->toArray();
  print Dumper({Array => $array}) if $UNIVERSAL::debug;
  my @allresults;
  foreach my $elt (@$array) {
    print "<$elt>\n" if $UNIVERSAL::debug;
    push @allresults, $self->ConvertJSONToPerl(Elt => $elt);
  }
  my $res1 = $self->TranslateResultBack
    (
     Results => \@allresults,
     Variables => $args{Variables},
    );
  print Dumper({Res1 => $res1}) if $UNIVERSAL::debug;
  return $res1;
}

sub DoQueryArgs {
  my ($self,%args) = @_;
  if ($args{Debug}) {
    $UNIVERSAL::debug = $args{Debug};
  }
  print Dumper({Query => $args{Query}}) if $UNIVERSAL::debug;
  print Dumper({FormalogPengines => $self->MyFormalogPengines}) if $UNIVERSAL::debug;
  my $res = $self->MyFormalogPengines->GetAllArgs
    (
     String => $args{Query},
     Server => $args{Server} || 'http://localhost:9881',
    );
  my $array = $res->toArray();
  print Dumper({Array => $array}) if $UNIVERSAL::debug;
  my @allresults;
  foreach my $elt (@$array) {
    print "<$elt>\n" if $UNIVERSAL::debug;
    push @allresults, $self->ConvertJSONToPerl(Elt => $elt);
  }
  my $res1 = $self->TranslateResultBack
    (
     Results => \@allresults,
     Variables => $args{Variables},
    );
  print Dumper({Res1 => $res1}) if $UNIVERSAL::debug;
  return $res1;
}

sub TranslateResultBack {
  my ($self,%args) = @_;
  print Dumper({Input => $args{Results}}) if $UNIVERSAL::debug;
  my @results;
  if ($args{Variables}) {
    push @results, '_prolog_list';
  }
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
	my $res1 = $self->ConvertJavaPengineToInterlingua(Entry => $value->{$r->{$key}});
	if ($res1->{Success}) {
	  $values->{$key} = $res1->{Output};
	} else {
	  # FIXME: confused, throw an error
	}
      }
    }
    print Dumper({Value => $value,
		  Values => $values}) if $UNIVERSAL::debug;
    if ($args{Variables}) {
      my @vars;
      my @values = ('_prolog_list');
      my $type = ref($args{Variables});
      if ($type eq 'GLOB') {
	push @vars, $args{Variables};
      } else {
	@vars = @{$args{Variables}};
      }
      foreach my $var (@vars) {
	if ($var ne '_prolog_list') {
	  print Dumper({Var => $var}) if $UNIVERSAL::debug;
	  my $varname = uc(DeVar($var));
	  $varname =~ s/^\?//;
	  print Dumper({Values => $values}) if $UNIVERSAL::debug;
	  print "<$varname>\n";
	  push @values, $values->{$varname};
	}
      }
      if ($type eq 'GLOB') {
	push @results, $values[1];
      } else {
	push @results, \@values;
      }
    } else {
      push @results, $values;
    }
  }
  print Dumper({Response => \@results}) if $UNIVERSAL::debug;
  return \@results;
}

sub ConvertJSONToPerl {
  my ($self,%args) = @_;
  print Dumper({Elt => $args{Elt}}) if $UNIVERSAL::debug;
  my $ds = from_json($args{Elt});
  # print Dumper($ds);
  return $ds;
}

sub ConvertJavaPengineToInterlingua {
  my ($self,%args) = @_;
  my @results;
  my $type = ref($args{Entry});
  if ($type eq 'ARRAY') {
    push @results, '_prolog_list';
    foreach my $subentry (@{$args{Entry}}) {
      my $res1 = $self->ConvertJavaPengineToInterlingua(Entry => $subentry);
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
	my $res1 = $self->ConvertJavaPengineToInterlingua(Entry => $args{Entry}{args});
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

sub Test {
  my ($self,%args) = @_;

  # DoQueryInterlingua(Input => [['member',Var('?X'),['_prolog_list',['a','taco',Var('?A'),Var('?_A'),'B',Var('?A'),'C D'],'b','dag',2,'2','c']]]);

  # my $query = "member(X, [a(taco,A,_A,'B','C D'),b,dag,2,'2',c])";
  # my $query = "(member(X,[a(taco,A,_A,'B',A,'C D'),b,dag,2,'2',c]),transform_result(X,Y))";

  # DoQuery(Query => "formalogPenginesCall(member([Y,_C],[[a,_C],[c,d],[e,f]]),MyResult)");
  # DoQuery(Query => "formalogPenginesCall(findall(X,member([Y,X],[[a,b],[c,d],[e,f]]),Xs),MyResult)");

  while (my $query = <>) {
    $self->DoQuery(Query => "formalogPenginesCall($query,MyResult).");
  }

  # my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]),Result)";
  # my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]))";
}

__PACKAGE__->meta->make_immutable;
1;

#!/usr/bin/env perl

use lib "/var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1";
# /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/ports.pl
# /var/lib/myfrdcsas/versions/myfrdcsa-1.0/sandbox-new/javapengine-20200506/javapengine-20200506/src/main/java/com/simularity/os/javapengine/example/ManualAsk.java
# cd /var/lib/myfrdcsa/sandbox/javapengine-20200506/javapengine-20200506 && ./frdcsa.sh
# cd /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/scripts/ && ./test.pl

# /var/lib/myfrdcsa/codebases/minor/formalog-pengines/attempts/pengines/1/FormalogPengines.pm

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
  print Dumper($fp);
  my $res = $fp->GetAll($args{Query});
  my $array = $res->toArray();
  print Dumper({Array => $array});
  foreach my $elt (@$array) {
    print "<$elt>\n";
    push @results, ConvertJSONToPerl(Elt => $elt);
  }
}

sub ConvertJSONToPerl {
  my (%args) = @_;
  print Dumper({Elt => $args{Elt}});
  my $ds = from_json($args{Elt});
  print Dumper($ds);
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

my $query = "member(X, [a(taco,A,_A,'B','C D'),b,dag,2,'2',c])";
# my $query = "(member(X,[a(taco,A,_A,'B',A,'C D'),b,dag,2,'2',c]),transform_result(X,Y))";

if (1) {
  DoQuery(Query => 'formalogPenginesCall(member(X,[a,b,1.2,2,a(b,c(A)),B,C,C]),MyResult)');
  # DoQuery(Query => 'formalogPenginesCall(listNotifications,MyResult)');
} else {
  DoQuery(Query => $query);
}

# my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]),Result)";
# my $query = "formalogPenginesCall(member(X, [1,'1',a,A,'A',[],B]))";

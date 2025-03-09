#!/usr/bin/env perl

use Formalog::Client; # ::Pengines;
use KBS2::Util;
use Data::Dumper;

$UNIVERSAL::debug = 1;

my $result1 = FormalogQuery
  (
   Agent => 'DMS-Formalog-Agent1',
   Vars => ['_prolog_list',Var('?Results')],
   # Query => ['dmsFormalogFlag',Var('?X')],
   # Query => ['view',['[|]',['log',['dog']]]],
   Query => ['dms_assert',['[|]',['query',['holds','Org::FRDCSA::PaperlessOffice::Cabinet::Test',['has-folder',['document-fn','Test','PUNs89XSbN'],'Incoming from Scanner']]]],Var('?_X')],
   # kassert_argt(AgentName,FormalogName,Arguments,_Result)
   # Query => ['kassert','DMS-Formalog-Agent1','DMS-Formalog-Yaswi1',['p','a'],Var('?_X')],
   # Query => ['kassert','Agent1','Yaswi1',['p','a'],Var('?_X')],
   # Query => ['assertz',['p','a']],
  );

# formalogPenginesCall(dms_assert('[|]'(query(holds('Org::FRDCSA::PaperlessOffice::Cabinet::Test','has-folder'('document-fn'('Test','PUNs89XSbN'),'Incoming from Scanner')))),RESULTS),MyResult)

print Dumper({DeleteResultFormalog => $result1});


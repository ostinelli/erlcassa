% ==========================================================================================================
% ERLCASSA - Cassandra Client
% 
% Copyright (C) 2011, Roberto Ostinelli <roberto@ostinelli.net>
% All rights reserved.
%
% The MIT License (MIT)
% 
% Copyright (c) 2011, Roberto Ostinelli <roberto@ostinelli.net>
% 
% Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
% associated documentation files (the "Software"), to deal in the Software without restriction, including
% without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
% following conditions:
% 
% The above copyright notice and this permission notice shall be included in all copies or substantial
% portions of the Software.
% 
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
% LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
% EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
% IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
% THE USE OR OTHER DEALINGS IN THE SOFTWARE.
% ==========================================================================================================
-module(erlcassa_client).
-vsn("0.1-dev").

% API
-export([connect/2, disconnect/1, cql_execute/2, cql_execute/3, get_column/2]).

% includes
-include("../include/cassandra_types.hrl").
-include("../include/erlcassa.hrl").

% ============================ \/ API ======================================================================

% connect
-spec connect(Host::string(), Port::non_neg_integer()) -> {ok, Client::term()} | {error, Reason::term()}.
connect(Host, Port) ->
	case catch thrift_client_util:new(Host, Port, cassandra_thrift, [{framed, true}]) of
		{ok, Client} ->
			% TODO: login / learn
			{ok, Client};					
		{'EXIT', {{_, {error, Reason}}, _}} ->
			{error, Reason}
	end.

% close connection
-spec disconnect(Client::term()) -> ok.
disconnect(Client) ->
	thrift_client:close(Client),
	ok.

% execute CQL query
-spec cql_execute(Client::term(), CqlQuery::string()) -> {error, Reason::term()} | {result, ok | {rows, Rows::list()}}.
-spec cql_execute(Client::term(), CqlQuery::string(), RowsFormat::rows_format()) -> {error, Reason::term()} | {result, ok | {rows, Rows::list()}}.
cql_execute(Client, CqlQuery) ->
	cql_execute(Client, CqlQuery, dict).
cql_execute(Client, CqlQuery, RowsFormat) when RowsFormat =:= dict; RowsFormat =:= proplist ->
	?LOG_DEBUG("executing cql query: ~p", [CqlQuery]),
	case catch thrift_client:call(Client, execute_cql_query, [CqlQuery, ?cassandra_Compression_NONE]) of
		{_Client1, {ok, #cqlResult{type = Type} = CqlResult}} ->
			case Type of
				?cassandra_CqlResultType_VOID ->
					?LOG_DEBUG("got void from cql query",[]),
					{result, ok};
				?cassandra_CqlResultType_ROWS ->
					?LOG_DEBUG("got row(s) from cql query", []),
					Rows = erlcassa_decoder:decode(rows, CqlResult, RowsFormat),
					{result, {rows, Rows}}
			end;		
		{_Client1, {exception, Exception}}	->
			?LOG_ERROR("exception while executing cql query [~p]: ~p", [CqlQuery, Exception]),
			{error, Exception}
	end.

-spec get_column(ColumnName::term(), Row::list() | dict()) -> {Name::term(), Value::term(), Timestamp::integer(), Ttl::undefined | integer()}.
get_column(ColumnName, Row) when is_list(Row) ->
	case lists:keyfind(ColumnName, 1, Row) of
		false-> undefined;
		Column -> Column
	end;	
get_column(ColumnName, Row) ->
	case dict:find(ColumnName, Row) of
		{ok, Column} ->	Column;
		error ->		undefined
	end.

% ============================ /\ API ======================================================================

% ============================ \/ INTERNAL FUNCTIONS =======================================================

% ============================ /\ INTERNAL FUNCTIONS =======================================================

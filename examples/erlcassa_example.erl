% ==========================================================================================================
% ERLCASSA - Cassandra Client Basic Example
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
-module(erlcassa_example).

% API
-compile(export_all).

% includes
-include("../include/erlcassa.hrl").


run() ->
	% connect to client
	{ok, C} = erlcassa_client:connect("localhost", 9160),
	io:format("Connected to Cassandra~n"),
	% create keyspace
	{result, ok} = erlcassa_client:cql_execute(C, "CREATE KEYSPACE test1 WITH strategy_class = SimpleStrategy AND strategy_options:replication_factor = 1;"),
	io:format("Created KeySpace test1~n"),
	% use keyspace
	erlcassa_client:cql_execute(C, "USE test1;"),
	% create column family
	{result, ok} = erlcassa_client:cql_execute(C, "
		CREATE COLUMNFAMILY testdata (
		KEY int PRIMARY KEY,
		text_field text,
		ascii_field ascii,
		bigint_field bigint,
		blob_field blob,
		boolean_field boolean,
		counter_field counter,
		decimal_field decimal,
		double_field double,
		float_field float,
		timestamp_field timestamp,
		uuid_field uuid,
		varchar_field varchar,
		varint_field varint);
	"),
	io:format("Created Column Family testdata~n"),
	% insert an entry in the column family
	{result, ok} = erlcassa_client:cql_execute(C, "
		INSERT INTO testdata (
			KEY, text_field, ascii_field, bigint_field, blob_field, boolean_field,
			decimal_field, double_field, float_field, timestamp_field, uuid_field,
			varchar_field, varint_field
		) VALUES (
			123, 'text', 'ascii', 1234567890, 'aaaaaa', true,
			42, 1.3513535135, 1.2345, 1324244361517000, 'f47ac10b-58cc-4372-a567-0e02b2c3d479',
			'encoded', 13
		);
	"),
	io:format("Inserted entry into Column Family testdata~n"),
	% get all rows from the column family, as proplist (dict is the other possible return value format)
	{result, {rows, Rows}} = erlcassa_client:cql_execute(C, "SELECT * FROM testdata", proplist),
	io:format("Retrieved all rows Column Family testdata, as proplist: ~p~n", [Rows]),
	% get first row
	[Row|_] = Rows,
	% return KEY value of first row
	KeyColumn = erlcassa_client:get_column("KEY", Row),
	io:format("Retrieved the first entry's key column: ~p~n", [KeyColumn]),
	% drop the column family
	{result, ok} = erlcassa_client:cql_execute(C, "DROP COLUMNFAMILY testdata;"),
	io:format("Dropped the Column Family testdata~n"),
	% drop the keyspace
	{result, ok} = erlcassa_client:cql_execute(C, "DROP KEYSPACE test1;"),
	io:format("Dropped the KeySpace test1~n").


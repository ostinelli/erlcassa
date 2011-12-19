% ==========================================================================================================
% ERLCASSA - Marshalling
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
-module(erlcassa_decoder).
-vsn("0.1-dev").

% API
-export([decode/3]).

% includes
-include("../include/cassandra_types.hrl").
-include("../include/erlcassa.hrl").


% ============================ \/ API ======================================================================
-spec decode(rows, CqlResult::#cqlResult{}, RowsFormat::rows_format()) -> [dict()] | [gen_proplist()].
decode(rows, CqlResult, RowsFormat) ->
	#cqlResult{rows = Rows, schema = CqlMetadata} = CqlResult,
	#cqlMetadata{
		name_types = NameTypes,
		value_types = ValueTypes,
		default_name_type = DefaultNameType,
		default_value_type = DefaultValueType
	} = CqlMetadata,
	% define default RowAcc depending on format
	DefaultRowAcc = default_row_acc(RowsFormat),
	% decode and build result	
	ColumnFun = fun(#column{name = ColName, value = ColValue, timestamp = ColTimestamp, ttl = ColTtl}, ColumnAcc) ->
		% get name type
		ColNameType = get_marshal(ColName, NameTypes, DefaultNameType),
		ColValueType = get_marshal(ColName, ValueTypes, DefaultValueType),
		% decode
		ColNameDecoded = i_decode(ColNameType, ColName),
		ColValueDecoded = i_decode(ColValueType, ColValue),
		% add column
		case RowsFormat of
			dict -> dict:store(ColNameDecoded, {ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl}, ColumnAcc);
			proplist -> [{ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl} | ColumnAcc]
		end
	end,
	RowFun = fun(#cqlRow{columns = Columns}, RowAcc) ->
		[lists:foldl(ColumnFun, DefaultRowAcc, Columns) | RowAcc]
	end,
	lists:foldl(RowFun, [], Rows).

% ============================ /\ API ======================================================================


% ============================ \/ INTERNAL FUNCTIONS =======================================================

-spec default_row_acc(RowsFormat::rows_format()) -> dict() | list().
default_row_acc(dict) -> dict:new();
default_row_acc(proplist) -> [].

% -spec add_column_result(
% 	RowsFormat::rows_format(),
% 	{ColNameDecoded::term(), ColValueDecoded::term(), ColTimestamp::integer(), ColTtl::undefined | integer()},
% 	ColumnAcc::dict() | list()) -> dict() | list().
% add_column_result(dict, {ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl}, ColumnAcc) ->
% 	dict:store(ColNameDecoded, {ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl}, ColumnAcc);
% add_column_result(proplist, {ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl}, ColumnAcc) ->
% 	[{ColNameDecoded, ColValueDecoded, ColTimestamp, ColTtl} | ColumnAcc].
	
-spec get_marshal(Name::binary(), Types::dict(), Default::binary()) -> binary().
get_marshal(Name, Types, Default) ->
	case dict:find(Name, Types) of
		error -> Default;
		{ok, Marshal} -> Marshal
	end.

% ---------------------------- \/ Marshal Decoders ---------------------------------------------------------

% Internal Type 	CQL Name 		Description
% BytesType 		blob 			Arbitrary hexadecimal bytes (no validation)
% AsciiType 		ascii 			US-ASCII character string
% UTF8Type 			text, varchar 	UTF-8 encoded string
% IntegerType 		varint 			Arbitrary-precision integer
% Int32Type			int			 	4-byte long
% LongType 			bigint		 	8-byte long
% UUIDType 			uuid 			Type 1 or type 4 UUID
% DateType 			timestamp 		Date plus time, encoded as 8 bytes since epoch
% BooleanType 		boolean 		true or false
% FloatType 		float 			4-byte floating point
% DoubleType 		double 			8-byte floating point
% DecimalType 		decimal 		Variable-precision decimal
% CounterColumnType counter 		Distributed counter value (8-byte long)

-spec i_decode(Type::binary(), Bytes::binary()) -> term().
i_decode(<<"AsciiType">>, Bytes) ->
	binary_to_list(Bytes);
i_decode(<<"UTF8Type">>, Bytes) ->
	unicode:characters_to_list(Bytes, utf8);
i_decode(<<"IntegerType">>, Bytes) ->
	Size = size(Bytes) * 8,
	<<Integer:Size/integer>> = Bytes,
	Integer;
i_decode(<<"Int32Type">>, <<Int:32/integer>>) ->
	Int;
i_decode(<<"LongType">>, <<Long:64/integer>>) ->
	Long;
i_decode(<<"UUIDType">>, <<TL:32, TM:16, THV:16, CSR:8, CSL:8, N:48>>) ->
	% UUID conversion from byte Copyright (c) 2008, Travis Vachon
	% <https://github.com/travis/erlang-uuid/blob/master/uuid.erl>
	lists:flatten(io_lib:format("~8.16.0b-~4.16.0b-~4.16.0b-~2.16.0b~2.16.0b-~12.16.0b", [TL, TM, THV, CSR, CSL, N]));
i_decode(<<"DateType">>, <<Timestamp:64/integer>>) ->
	Timestamp;
i_decode(<<"BooleanType">>, Bytes) ->
	case Bytes of
		<<0>> -> false;
		<<1>> -> true
	end;
i_decode(<<"FloatType">>, <<Float:32/float>>) ->
	Float;
i_decode(<<"DoubleType">>, <<Double:64/float>>) ->
	Double;
i_decode(<<"DecimalType">>, Bytes) ->
	Size = size(Bytes) * 8,
	<<Decimal:Size/integer>> = Bytes,
	Decimal;
i_decode(<<"CounterColumnType">>, <<Counter:64/integer>>) ->
	Counter;
i_decode(_, Bytes) ->
	Bytes.
% ---------------------------- /\ Marshal Decoders ---------------------------------------------------------

% ============================ /\ INTERNAL FUNCTIONS =======================================================

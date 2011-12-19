## ErlCassa

ErlCassa is an Erlang client to the Cassandra Query Language (CQL). This is an alpha library which should not be used in production.

## About CQL

[Cassandra](http://cassandra.apache.org/) originally went with a Thrift RPC-based API as a way to provide a common denominator that more idiomatic clients could build upon independently. However, this worked poorly in practice: raw Thrift is too low-level to use productively, and keeping pace with new API methods to support (for example) indexes in 0.7 or distributed counters in 0.8 is too much for many maintainers to keep pace with.

CQL, the Cassandra Query Language, addresses this by pushing all implementation details to the server; all the client has to know for any operation is how to interpret “resultset” objects. So adding a feature like counters just requires teaching the CQL parser to understand “column + N” notation; no client-side changes are necessary.

CQL specification can be read here: http://github.com/apache/cassandra/blob/trunk/doc/cql/CQL.textile

## Install Dependencies

* You will need [Apache Thrift](http://thrift.apache.org/download/) >= 0.80 and [Apache Cassandra](http://cassandra.apache.org/) >= 1.0.

### Note for OSX developers

The easiest way to install Thrift on your system is to:

* [install MacPorts](http://www.macports.org/install.php);
* Download and install Thrift dependencies using MacPorts by issuing the command:

```bash
$ sudo port install boost libevent pkgconfig
```

* [Download Apache Thrift](http://thrift.apache.org/download/), then compile and install it:

```bash
$ ./configure --prefix=/usr/local/ --with-boost=/opt/local --with-libevent=/opt/local \
  --without-csharp --without-ruby --without-perl --without-haskell
$ make
$ sudo make install
```

## Quick Start

### Connecting to Cassandra

```erlang
{ok, C} = erlcassa_client:connect("localhost", 9160).
```

### Creating a Keyspace

```erlang
{result, ok} = erlcassa_client:cql_execute(C,
    "CREATE KEYSPACE test1 WITH strategy_class = SimpleStrategy AND strategy_options:replication_factor = 1;"
).
```

### Using a Keyspace

```erlang
{result, ok} = erlcassa_client:cql_execute(C, "USE test1;").
```

### Creating a Column Family

```erlang
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
").
```

### Inserting an entry in a Column Family

```erlang
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
").
```

### Getting rows from a Column Family, as proplist (dict is the other possible return value format)

```erlang
{result, {rows, Rows}} = erlcassa_client:cql_execute(C, "SELECT * FROM testdata", proplist).
```

### Returning an entire Column from an individual Row

```erlang
Column = erlcassa_client:get_column("KEY", Row).
```

### Dropping a Column Family

```erlang
{result, ok} = erlcassa_client:cql_execute(C, "DROP COLUMNFAMILY testdata;").
```

### Dropping a Keyspace

```erlang
{result, ok} = erlcassa_client:cql_execute(C, "DROP KEYSPACE test1;").
```

## License

The MIT License (MIT)

Copyright (c) 2011, Roberto Ostinelli

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
associated documentation files (the "Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the
following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial
portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT
LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO
EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR
THE USE OR OTHER DEALINGS IN THE SOFTWARE.
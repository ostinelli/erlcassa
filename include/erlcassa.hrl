% ==========================================================================================================
% ERLCASSA - Include file
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


% ============================ \/ LOG ======================================================================
-ifdef(log_debug).
-define(LOG_DEBUG(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["[DEBUG]	pid: ", pid_to_list(self()), "~n	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_info).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), erlang:apply(error_logger, info_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
-ifdef(log_error).
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), ok).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-else.
% default to warning level
-define(LOG_DEBUG(Str, Args), ok).
-define(LOG_INFO(Str, Args), ok).
-define(LOG_WARNING(Str, Args), erlang:apply(error_logger, warning_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-define(LOG_ERROR(Str, Args), erlang:apply(error_logger, error_msg, [lists:concat(["	module: ", ?MODULE, "~n	line: ", ?LINE, "~n", Str, "~n"]), Args])).
-endif.
-endif.
-endif.
% ============================ /\ LOG ======================================================================


% ============================ \/ TYPES ====================================================================
-type gen_proplist() :: [{Tag::atom()|list()|binary(), Value::term()}].
-type rows_format() :: dict | proplist.
% ============================ /\ TYPES ====================================================================

% ============================ \/ RECORDS ==================================================================
% ============================ /\ RECORDS ==================================================================

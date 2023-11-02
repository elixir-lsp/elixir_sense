% This file includes modified code extracted from the elixir project. Namely:
%
% https://github.com/elixir-lang/elixir/blob/v1.13.4/lib/elixir/src/elixir.hrl
%
% The original code is licensed as follows:
%
% Copyright 2012 Plataformatec
% Copyright 2021 The Elixir Team
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
%
%    https://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

% The only changes here are module renames

-define(key(M, K), maps:get(K, M)).
-define(ann(Meta), elixir_erl:get_ann(Meta)).
-define(line(Meta), elixir_utils:get_line(Meta)).
-define(generated(Meta), [{generated, true} | Meta]).
-define(var_context, ?MODULE).
-define(remote(Ann, Module, Function, Args), {call, Ann, {remote, Ann, {atom, Ann, Module}, {atom, Ann, Function}}, Args}).

-record(elixir_ex, {
  caller=false,            %% stores if __CALLER__ is allowed
  prematch=warn,           %% {Read, Counter} | warn | raise | pin
  stacktrace=false,        %% stores if __STACKTRACE__ is allowed
  unused={#{}, 0},         %% a map of unused vars and a version counter for vars
  vars={#{}, false}        %% a tuple with maps of read and optional write current vars
}).

-record(elixir_erl, {
  context=nil,             %% can be match, guards or nil
  extra=nil,               %% extra information about the context, like pin_guard and map_key
  caller=false,            %% when true, it means caller was invoked
  var_names=#{},           %% maps of defined variables and their alias
  extra_guards=[],         %% extra guards from args expansion
  counter=#{},             %% a map counting the variables defined
  expand_captures=false,   %% a boolean to control if captures should be expanded
  stacktrace=nil           %% holds information about the stacktrace variable
}).

-record(elixir_tokenizer, {
  file=(<<"nofile">>),
  terminators=[],
  unescape=true,
  cursor_completion=false,
  existing_atoms_only=false,
  static_atoms_encoder=nil,
  preserve_comments=nil,
  identifier_tokenizer=elixir_tokenizer,
  indentation=0,
  mismatch_hints=[],
  warn_on_unnecessary_quotes=true,
  warnings=[]
}).

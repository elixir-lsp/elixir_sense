defmodule ElixirSense.Providers.SuggestionTest do

  use ExUnit.Case
  alias ElixirSense.Providers.Suggestion

  defmodule MyModule do
    def say_hi, do: true
  end

  test "find definition of functions from Kernel" do
      assert Suggestion.find("List", [], [], [], [], [], SomeModule) == [
        %{type: :hint, value: "List."},
        %{name: "List", subtype: nil, summary: "Specialized functions that only work on lists.", type: :module},
        %{name: "Chars", subtype: :protocol, summary: "The List.Chars protocol is responsible for\\nconverting a structure to a list (only if applicable).\\nThe only function required to be implemented is\\n`to_charlist` which does the conversion.", type: :module},
        %{args: "", arity: 1, name: "__info__", origin: "List", spec: nil, summary: "", type: "function"},
        %{args: "list", arity: 1, name: "first", origin: "List", spec: "@spec first([elem]) :: nil | elem when elem: var", summary: "Returns the first element in `list` or `nil` if `list` is empty.", type: "function"},
        %{args: "list", arity: 1, name: "last", origin: "List", spec: "@spec last([elem]) :: nil | elem when elem: var", summary: "Returns the last element in `list` or `nil` if `list` is empty.", type: "function"},
        %{args: "charlist", arity: 1, name: "to_atom", origin: "List", spec: "@spec to_atom(charlist) :: atom", summary: "Converts a charlist to an atom.", type: "function"},
        %{args: "charlist", arity: 1, name: "to_existing_atom", origin: "List", spec: "@spec to_existing_atom(charlist) :: atom", summary: "Converts a charlist to an existing atom. Raises an `ArgumentError`\\nif the atom does not exist.", type: "function"},
        %{args: "charlist", arity: 1, name: "to_float", origin: "List", spec: "@spec to_float(charlist) :: float", summary: "Returns the float whose text representation is `charlist`.", type: "function"},
        %{args: "list", arity: 1, name: "to_string", origin: "List", spec: "@spec to_string(:unicode.charlist) :: String.t", summary: "Converts a list of integers representing codepoints, lists or\\nstrings into a string.", type: "function"},
        %{args: "list", arity: 1, name: "to_tuple", origin: "List", spec: "@spec to_tuple(list) :: tuple", summary: "Converts a list to a tuple.", type: "function"},
        %{args: "list", arity: 1, name: "wrap", origin: "List", spec: "@spec wrap(list | any) :: list", summary: "Wraps the argument in a list.\\nIf the argument is already a list, returns the list.\\nIf the argument is `nil`, returns an empty list.", type: "function"},
        %{args: "list_of_lists", arity: 1, name: "zip", origin: "List", spec: "@spec zip([list]) :: [tuple]", summary: "Zips corresponding elements from each list in `list_of_lists`.", type: "function"},
        %{args: "", arity: 1, name: "module_info", origin: "List", spec: nil, summary: "", type: "function"},
        %{args: "", arity: 0, name: "module_info", origin: "List", spec: nil, summary: "", type: "function"},
        %{args: "list,item", arity: 2, name: "delete", origin: "List", spec: "@spec delete(list, any) :: list", summary: "Deletes the given item from the list. Returns a list without\\nthe item. If the item occurs more than once in the list, just\\nthe first occurrence is removed.", type: "function"},
        %{args: "list,index", arity: 2, name: "delete_at", origin: "List", spec: "@spec delete_at(list, integer) :: list", summary: "Produces a new list by removing the value at the specified `index`.\\nNegative indices indicate an offset from the end of the list.\\nIf `index` is out of bounds, the original `list` is returned.", type: "function"},
        %{args: "elem,n", arity: 2, name: "duplicate", origin: "List", spec: "@spec duplicate(elem, non_neg_integer) :: [elem] when elem: var", summary: "Duplicates the given element `n` times in a list.", type: "function"},
        %{args: "list,position", arity: 2, name: "keysort", origin: "List", spec: "@spec keysort([tuple], non_neg_integer) :: [tuple]", summary: "Receives a list of tuples and sorts the items\\nat `position` of the tuples. The sort is stable.", type: "function"},
        %{args: "list,tail", arity: 2, name: "flatten", origin: "List", spec: "@spec flatten(deep_list, [elem]) :: [elem] when deep_list: [elem | deep_list], elem: var", summary: "Flattens the given `list` of nested lists.\\nThe list `tail` will be added at the end of\\nthe flattened list.", type: "function"},
        %{args: "list", arity: 1, name: "flatten", origin: "List", spec: "@spec flatten(deep_list) :: list when deep_list: [any | deep_list]", summary: "Flattens the given `list` of nested lists.", type: "function"},
        %{args: "charlist,base", arity: 2, name: "to_integer", origin: "List", spec: "@spec to_integer(charlist, 2..36) :: integer", summary: "Returns an integer whose text representation is `charlist` in base `base`.", type: "function"},
        %{args: "charlist", arity: 1, name: "to_integer", origin: "List", spec: "@spec to_integer(charlist) :: integer", summary: "Returns an integer whose text representation is `charlist`.", type: "function"},
        %{args: "list,acc,function", arity: 3, name: "foldl", origin: "List", spec: "@spec foldl([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var", summary: "Folds (reduces) the given list from the left with\\na function. Requires an accumulator.", type: "function"},
        %{args: "list,acc,function", arity: 3, name: "foldr", origin: "List", spec: "@spec foldr([elem], acc, (elem, acc -> acc)) :: acc when elem: var, acc: var", summary: "Folds (reduces) the given list from the right with\\na function. Requires an accumulator.", type: "function"},
        %{args: "list,index,value", arity: 3, name: "insert_at", origin: "List", spec: "@spec insert_at(list, integer, any) :: list", summary: "Returns a list with `value` inserted at the specified `index`.\\nNote that `index` is capped at the list length. Negative indices\\nindicate an offset from the end of the list.", type: "function"},
        %{args: "list,key,position", arity: 3, name: "keydelete", origin: "List", spec: "@spec keydelete([tuple], any, non_neg_integer) :: [tuple]", summary: "Receives a list of tuples and deletes the first tuple\\nwhere the item at `position` matches the\\ngiven `key`. Returns the new list.", type: "function"},
        %{args: "list,key,position", arity: 3, name: "keymember?", origin: "List", spec: "@spec keymember?([tuple], any, non_neg_integer) :: any", summary: "Receives a list of tuples and returns `true` if there is\\na tuple where the item at `position` in the tuple matches\\nthe given `key`.", type: "function"},
        %{args: "list,key,position", arity: 3, name: "keytake", origin: "List", spec: "@spec keytake([tuple], any, non_neg_integer) :: {tuple, [tuple]} | nil", summary: "Receives a `list` of tuples and returns the first tuple\\nwhere the element at `position` in the tuple matches the\\ngiven `key`, as well as the `list` without found tuple.", type: "function"},
        %{args: "list,index,value", arity: 3, name: "replace_at", origin: "List", spec: "@spec replace_at(list, integer, any) :: list", summary: "Returns a list with a replaced value at the specified `index`.\\nNegative indices indicate an offset from the end of the list.\\nIf `index` is out of bounds, the original `list` is returned.", type: "function"},
        %{args: "list,index,fun", arity: 3, name: "update_at", origin: "List", spec: "@spec update_at([elem], integer, (elem -> any)) :: list when elem: var", summary: "Returns a list with an updated value at the specified `index`.\\nNegative indices indicate an offset from the end of the list.\\nIf `index` is out of bounds, the original `list` is returned.", type: "function"},
        %{args: "list,key,position,default \\\\ nil", arity: 4, name: "keyfind", origin: "List", spec: "@spec keyfind([tuple], any, non_neg_integer, any) :: any", summary: "Receives a list of tuples and returns the first tuple\\nwhere the item at `position` in the tuple matches the\\ngiven `key`.", type: "function"},
        %{args: "list,key,position,new_tuple", arity: 4, name: "keyreplace", origin: "List", spec: "@spec keyreplace([tuple], any, non_neg_integer, tuple) :: [tuple]", summary: "Receives a list of tuples and replaces the item\\nidentified by `key` at `position` if it exists.", type: "function"},
        %{args: "list,key,position,new_tuple", arity: 4, name: "keystore", origin: "List", spec: "@spec keystore([tuple], any, non_neg_integer, tuple) :: [tuple, ...]", summary: "Receives a list of tuples and replaces the item\\nidentified by `key` at `position`. If the item\\ndoes not exist, it is added to the end of the list.", type: "function"}
      ]
  end

  test "return completion candidates for 'Str'" do
    assert Suggestion.find("Str", [], [], [], [], [], SomeModule) == [
      %{type: :hint, value: "Str"},
      %{name: "Stream", subtype: :struct, summary: "Module for creating and composing streams.", type: :module},
      %{name: "String", subtype: nil, summary: "A String in Elixir is a UTF-8 encoded binary.", type: :module},
      %{name: "StringIO", subtype: nil, summary: "Controls an IO device process that wraps a string.", type: :module}
    ]
  end

  test "return completion candidates for 'List.del'" do
    assert Suggestion.find("List.del", [], [], [], [], [], SomeModule) == [
      %{type: :hint, value: "List.delete"},
      %{args: "list,item", arity: 2, name: "delete", origin: "List", spec: "@spec delete(list, any) :: list", summary: "Deletes the given item from the list. Returns a list without\\nthe item. If the item occurs more than once in the list, just\\nthe first occurrence is removed.", type: "function"},
      %{args: "list,index", arity: 2, name: "delete_at", origin: "List", spec: "@spec delete_at(list, integer) :: list", summary: "Produces a new list by removing the value at the specified `index`.\\nNegative indices indicate an offset from the end of the list.\\nIf `index` is out of bounds, the original `list` is returned.", type: "function"}
    ]
  end

  test "return completion candidates for module with alias" do
    assert Suggestion.find("MyList.del", [], [{MyList, List}], [], [], [], SomeModule) == [
      %{type: :hint, value: "MyList.delete"},
      %{args: "list,item", arity: 2, name: "delete", origin: "List", spec: "@spec delete(list, any) :: list", summary: "Deletes the given item from the list. Returns a list without\\nthe item. If the item occurs more than once in the list, just\\nthe first occurrence is removed.", type: "function"},
      %{args: "list,index", arity: 2, name: "delete_at", origin: "List", spec: "@spec delete_at(list, integer) :: list", summary: "Produces a new list by removing the value at the specified `index`.\\nNegative indices indicate an offset from the end of the list.\\nIf `index` is out of bounds, the original `list` is returned.", type: "function"}
    ]
  end

  test "return completion candidates for functions from import" do
    assert Suggestion.find("say", [MyModule], [], [], [], [], SomeModule) == [
      %{type: :hint, value: "say"},
      %{args: "", arity: 0, name: "say_hi", origin: "ElixirSense.Providers.SuggestionTest.MyModule", spec: "", summary: "", type: "public_function"}
    ]
  end

end

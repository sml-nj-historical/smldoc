(* list-sort.sig
 *
 * COPYRIGHT (c) 2014 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

(** The interface to an implementation of list sorting
 ** @author Johh Reppy
 ** @version 1.0
 ** @instance ListMergeSort
 ** @instance ListQuickSort
 *)
signature LIST_SORT =
  sig

  (** [sort gt lst] sorts the list [lst] in increasing order according to the {i greater-than}
   ** predicate [gt].
   ** @param gt tests if its first argument is greater than its second argument
   ** @param lst the list to be sorted
   ** @return the sorted list
   *)
    val sort : ('a * 'a -> bool) -> 'a list -> 'a list
 
  (** [uniqueSort cmp lst] sorts the list [lst] in increasing order according to the comparison
   ** function [cmp] while removing duplicate elements.
   ** @param cmp a comparison on the list element type
   ** @param lst the list to be sorted
   ** @return the sorted list with duplicates removed
   *)
    val uniqueSort : ('a * 'a -> order) -> 'a list -> 'a list

  (** [sorted gt lst] tests to see if the list [lst] is sorted in increasing order.
   ** @param gt tests if its first argument is greater than its second argument
   ** @param lst the list to be tested
   ** @return [true] if the list is sorted, [false] otherwise
   *)
    val sorted : ('a * 'a -> bool) -> 'a list -> bool

  end

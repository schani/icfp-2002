type priority = int
type 'a queue

exception Queue_is_empty

val empty : 'a queue
val emptyP : 'a queue -> bool

val insert : 'a queue -> priority -> 'a -> 'a queue
val remove_top : 'a queue -> 'a queue
val extract : 'a queue -> priority * 'a * 'a queue

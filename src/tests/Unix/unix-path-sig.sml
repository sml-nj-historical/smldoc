(* unix-path-sig.sml
 *
 * COPYRIGHT (c) 2007 The Fellowship of SML/NJ (http://smlnj.org)
 * All rights reserved.
 *)

(** Support for finding things in a Unix file system.
 **
 ** @author John Reppy
 ** @date 2014-11-17
 ** @version 1.0
 ** @instance UnixPath
 *)
signature UNIX_PATH =
  sig

  (** a list of paths to search for executables *)
    type path_list = string list

    val getPath : unit -> path_list
	(**< get the user's PATH environment variable. *)

  (** File access modes: [A_EXEC], [A_READ], [A_WRITE] *)
    datatype access_mode = datatype OS.FileSys.access_mode

  (** the different types of Unix file-system objects *)
    datatype file_type
      = F_REGULAR	(**< regular file *)
      | F_DIR		(**< directory file *)
      | F_SYMLINK	(**< symbolic link *)
      | F_SOCK		(**< socket *)
      | F_CHR		(**< character device *)
      | F_BLK		(**< block device *)

  (** raised when an attempt to find a file fails *)
    exception NoSuchFile

  (** [findFile (paths, mode) name]
   ** returns the string [p/name], where [p] is the first path in the path list
   ** [paths] such that [p/name] has the given access modes.
   *)
    val findFile : (path_list * access_mode list) -> string -> string

  (** [findFiles (paths, mode) name]
   ** returns a list of strings of the form [p/name], such that [p] is in the list of
   ** paths and [p/name] has the specified access modes.
   *)
    val findFiles : (path_list * access_mode list) -> string -> string list

  (** [findFile (paths, mode) name]
   ** returns the string [p/name], where [p] is the first path in the path list
   ** [paths] such that [p/name] has the given file type.
   *)
    val findFileOfType : (path_list * file_type * access_mode list) -> string -> string

  (** [findFiles (paths, mode) name]
   ** returns a list of strings of the form [p/name], such that [p] is in the list of
   ** paths and [p/name] has the given file type.
   *)
    val findFilesOfType : (path_list * file_type * access_mode list) -> string -> string list

  end (* UNIX_PATH *)

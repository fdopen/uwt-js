// Uwt
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, with linking exception;
// either version 2.1 of the License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.


// There are three operations on inet_addr (strings) inside unix.ml we are replacing
// 1) unix_inet_addr_of_string - already provided by stdlib.js with a stupid dummy function
//    for technical reasons (see: https://github.com/ocsigen/js_of_ocaml/issues/34 ). We will
//    just override it ;)
// 2) unix_string_of_inet_addr
// 3) is_inet6_addr, an internal caml function - not a cstub - used by Unix.domain_of_sockaddr.
//    By setting the property l, we will mislead caml_ml_string_length :D

//Provides: uwt_js_handle_exn
var uwt_js_handle_exn;

//Provides: unix_string_of_inet_addr
//Requires: caml_failwith, unix_inet_addr_of_string, caml_js_from_string, caml_js_to_string, uwt_js_handle_exn
var unix_string_of_inet_addr = (function(){
  var ipl;
  unix_inet_addr_of_string = function(o){
    if (!ipl){
      ipl = joo_global_object.process.binding("cares_wrap").isIP;
    }
    var i = caml_js_from_string(o);
    var n = ipl(i);
    var l;
    if ( n === 4 ){
      l = 4;
    }
    else if ( n === 6 ){
      l = 16;
    }
    else {
      caml_failwith("inet_addr_of_string");
    }
    return { l : l , i : i }
  };
  return (function(o){
    if ( typeof o === 'function' ){
      uwt_js_handle_exn = o;
      return (caml_js_to_string(""));
    }
    return (caml_js_to_string(o.i));
  });
})();

//Provides: unix_getpid
function unix_getpid(){
  return (joo_global_object.process.pid);
}

//Provides: unix_getuid
//Requires: uwt_js_handle_exn
var unix_getuid = (function(){
  if ( joo_global_object.process.platform === "win32" ){
    return function(){
      return 1;
    };
  }
  else {
    return function(){
      var uid;
      try {
        uid = joo_global_object.process.getuid();
      }
      catch (e) {
        if (e && typeof e === 'object' && uwt_js_handle_exn !== undefined){
          uwt_js_handle_exn(e);
        }
        else {
          throw e;
        }
      }
      return uid;
    };
  }})();

//Provides: unix_getgid
//Requires: uwt_js_handle_exn
var unix_getgid = (function(){
  if ( joo_global_object.process.platform === "win32" ){
    return function(){
      return 1;
    };
  }
  else {
    return function(){
      var gid;
      try {
        gid = joo_global_object.process.getgid();
      }
      catch (e) {
        if (e && typeof e === 'object' && uwt_js_handle_exn !== undefined){
          uwt_js_handle_exn(e);
        }
        else {
          throw e;
        }
      }
      return gid;
    };
  }})();

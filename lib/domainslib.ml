module Chan = Chan
module Task = Task

(* Heartbeat module - needed for install_state in spawned tasks *)
module Heartbeat = Heartbeat

(* Internal heartbeat module - exposed only for debugging/testing *)
module Heartbeat_internal = Heartbeat

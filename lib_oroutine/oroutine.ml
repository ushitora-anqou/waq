let traceln = Util.traceln

module Runtime = Runtime.Make (Global_run_queue_scheduler)
include Runtime
module Buffered_reader = Buffered_reader.Make (Runtime)

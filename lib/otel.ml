let with_setup f =
  if Config.(is_enabled (enable_otel ())) then (
    Opentelemetry.Globals.service_name := "waq";
    (*Opentelemetry.GC_metrics.basic_setup ();*)
    Ambient_context.set_current_storage Ambient_context_eio.storage;
    Opentelemetry_client_ocurl.with_setup () @@ fun () -> f ())
  else f ()

let with_span ?attrs ~__FUNCTION__ f =
  Opentelemetry.Tracer.with_ ?attrs __FUNCTION__ f

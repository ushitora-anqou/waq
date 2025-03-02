let with_setup f =
  Opentelemetry.Globals.service_name := "waq";
  (*Opentelemetry.GC_metrics.basic_setup ();*)
  Opentelemetry_ambient_context.set_storage_provider
    (Opentelemetry_ambient_context_eio.storage ());
  Opentelemetry_client_ocurl.with_setup () @@ fun () ->
  (*Opentelemetry_trace.setup ();*)
  f ()

let with_span ?attrs ~__FUNCTION__ f =
  Opentelemetry.Trace.with_ ?attrs __FUNCTION__ f

let with_span ?attrs ~__FUNCTION__ f =
  Opentelemetry.Tracer.with_ ?attrs __FUNCTION__ f

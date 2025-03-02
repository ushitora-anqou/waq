let with_span ?attrs ~__FUNCTION__ f =
  Opentelemetry.Trace.with_ ?attrs __FUNCTION__ f

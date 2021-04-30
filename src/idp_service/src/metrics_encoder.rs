use std::io;

/// MetricsEncoder provides methods to encode metrics in a text format
/// that can be understood by Prometheus.
///
/// See [Exposition Formats][1] for an informal specification of the text format.
///
/// [1] https://github.com/prometheus/docs/blob/master/content/docs/instrumenting/exposition_formats.md
pub struct MetricsEncoder<W: io::Write> {
    writer: W,
}

impl<W: io::Write> MetricsEncoder<W> {
    /// Constructs a new encoder dumping text into the specified
    /// writer.
    pub fn new(writer: W) -> Self {
        Self { writer }
    }

    /// Returns the internal buffer that was used to record the
    /// metrics.
    pub fn into_inner(self) -> W {
        self.writer
    }

    fn encode_header(&mut self, name: &str, help: &str, typ: &str) -> io::Result<()> {
        writeln!(self.writer, "# HELP {} {}", name, help)?;
        writeln!(self.writer, "# TYPE {} {}", name, typ)
    }

    /// Encodes the metadata and the value of a histogram.
    ///
    /// SUM is the sum of all observed values, before they were put
    /// into buckets.
    ///
    /// BUCKETS is a list (key, value) pairs, where KEY is the bucket
    /// and VALUE is the number of items *in* this bucket (i.e., it's
    /// not a cumulative value).
    pub fn encode_histogram(
        &mut self,
        name: &str,
        buckets: impl Iterator<Item = (f64, f64)>,
        sum: f64,
        help: &str,
    ) -> io::Result<()> {
        self.encode_header(name, help, "histogram")?;
        let mut total: f64 = 0.0;
        let mut saw_infinity = false;
        for (bucket, v) in buckets {
            total += v;
            if bucket == std::f64::INFINITY {
                saw_infinity = true;
                writeln!(self.writer, "{}_bucket{{le=\"+Inf\"}} {}", name, total)?;
            } else {
                writeln!(
                    self.writer,
                    "{}_bucket{{le=\"{}\"}} {}",
                    name, bucket, total
                )?;
            }
        }
        if !saw_infinity {
            writeln!(self.writer, "{}_bucket{{le=\"+Inf\"}} {}", name, total)?;
        }
        writeln!(self.writer, "{}_sum {}", name, sum)?;
        writeln!(self.writer, "{}_count {}", name, total)?;
        writeln!(self.writer)
    }

    pub fn encode_single_value(
        &mut self,
        typ: &str,
        name: &str,
        value: f64,
        help: &str,
    ) -> io::Result<()> {
        self.encode_header(name, help, typ)?;
        writeln!(self.writer, "{} {}", name, value)?;
        writeln!(self.writer)
    }

    /// Encodes the metadata and the value of a counter.
    pub fn encode_counter(&mut self, name: &str, value: f64, help: &str) -> io::Result<()> {
        self.encode_single_value("counter", name, value, help)
    }

    /// Encodes the metadata and the value of a gauge.
    pub fn encode_gauge(&mut self, name: &str, value: f64, help: &str) -> io::Result<()> {
        self.encode_single_value("gauge", name, value, help)
    }
}

#[cfg(test)]
mod test;

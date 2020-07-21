#!/usr/bin/gawk -f

# Parses the duration that each test took to run and outputs the runtime in milliseconds followed by
# a space and then the standard output for that test.
#
# This is useful for sorting the test output by test duration and seeing which tests are taking the longest.
#
# Example usage:
# sbt buildJVM | tee test-output.txt
# gawk -f scripts/parse-test-durations.awk test-output.txt | sort -k 1 -n
#
# Example output:
# 2185 [info] - FreeInvariantMonoidal[CsvCodec, *].invariantMonoidal.invariant monoidal right identity (2 seconds, 185 milliseconds)
# 2202 [info] - FreeInvariantMonoidal[CsvCodec, *].invariantMonoidal.invariant monoidal left identity (2 seconds, 202 milliseconds)
# 4778 [info] - Cokleisli[NonEmptyList, Int, Int].arrow.compose associativity (4 seconds, 778 milliseconds)
# 4781 [info] - Cokleisli[List, Int, Int].semigroupK.semigroupK associative (4 seconds, 781 milliseconds)
# 4824 [info] - Cokleisli[NonEmptyList, Int, Int].monoidK.semigroupK associative (4 seconds, 824 milliseconds)
#
# Note, this requires gawk-specific match functionality and may not work with all awk
# implementations.

# matches one of the following two formats:
# (73 milliseconds)
# (2 seconds, 42 milliseconds)
match($0, /\(([0-9]+) ([^),]+)(, ([0-9]+) ([^)]+))?/, a) {
  duration = a[1] * duration_factor(a[2]) + a[4] * duration_factor(a[5])
  print duration, $0
}

function duration_factor(duration_unit) {
  if (duration_unit ~ /^millisecond/)
    return 1
  if (duration_unit ~ /^second/)
    return 1000
}

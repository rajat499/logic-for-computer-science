val args = CommandLine.arguments()
val str = hd args
val _ = printResults(str)
val _ = OS.Process.exit(OS.Process.success)
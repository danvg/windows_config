$ErrorActionPreference = "Stop"

$jar_file = Get-ChildItem -Path "mod-loader-*.jar" | Sort-Object Name -Descending | Select-Object -First 1

$java_bin = "java.exe"
if ([System.Environment]::GetEnvironmentVariable("JAVA_HOME")) {
  $java_bin = $env:JAVA_HOME + "/bin/java.exe"
}

function ModLoader {
  param(
    [Parameter(
      ParameterSetName="Operation",
      Mandatory=$true
    )]
    [ArgumentCompletions("Query", "Add", "Remove", "Load", "Serve", "Help")]
    [string]$Operation
  )

  begin {
    switch ($Operation) {
      "Query" {
        & "$($java_bin)" -jar $jar_file query | ConvertFrom-Json
      }
      "Add" {
        & "$($java_bin)" -jar $jar_file add
      }
      "Remove" {
        & "$($java_bin)" -jar $jar_file remove
      }
      "Load" {
        & "$($java_bin)" -jar $jar_file load
      }
      "Serve" {
        & "$($java_bin)" -jar $jar_file serve
      }
      "Help" {
        & "$($java_bin)" -jar $jar_file --help
      }
    }
  }
}

# tj3-pert

Using the following example you can set PERT estimates on your TJ3 file.

```
      task opm "OPM Related Tasks" {
        task xml "Additional XML Middleware requirement" {
          new:PERT 3.5h 1d 7d
          new:allocate backend
          Jira 199
        }
      }
```

These are converted to the following.

```
      task opm "OPM Related Tasks" {
        task xml "Additional XML Middleware requirement" {
          new:effort 1.858d new:PERTMin 3.5h new:PERTProbable 1.0d new:PERTMax 7.0d new:PERTCriticalPath 1.858d new:PERTStandardDeviation 98700 new:PERTVariance 9741690000 
          new:allocate backend
          Jira 199
        }
      }
```

You need to add the following additional fields to your Task Juggler project header.  The jira custom fields are not required but are there as an example.

```
  # You can define your own attributes for tasks and resources. This
  # is handy to capture additional information about the project that
  # is not directly impacting the project schedule, but which you like to
  # keep in one place.
  extend task {
    # reference spec "Link to Wiki page"
    number Jira "Jira Ticket Number"
    reference JiraProject "Jira Project" { inherit }
    reference JiraURLPrefix "Jira URL Prefix" { inherit }
    number PERTMin "PERT Optimistic Estimate in Seconds" { scenariospecific }
    number PERTMax "PERT Pessimistic Estimate in Seconds" { scenariospecific }
    number PERTProbable "PERT Most Likely Estimate in Seconds" { scenariospecific }
    number PERTCriticalPath "PERT Critical Path in Seconds" { scenariospecific }
  }
```

Standard out is piped through from standard in.

Build with stack build.

```
stack build
```

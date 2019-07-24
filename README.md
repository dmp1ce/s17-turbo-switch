# s17-turbo-switch

[![CircleCI](https://circleci.com/gh/dmp1ce/s17-turbo-switch.svg?style=svg)](https://circleci.com/gh/dmp1ce/s17-turbo-switch)

Command line utility for remotely changing work mode for S17.

```
s17-switch-mode - Switch work mode on S17 miner

Usage: s17-turbo-switch [-v|--version] (-H|--host HOST) [-u|--username USERNAME]
                        [-p|--password PASSWORD] [-m|--workmode MODE]
  Switch work mode on S17 miner

Available options:
  -h,--help                Show this help text
  -v,--version             Show version information
  -H,--host HOST           Hostname (or IP) of S17 host
  -u,--username USERNAME   username to log into SSH (default: "root")
  -p,--password PASSWORD   password to log into SSH (default: "admin")
  -m,--workmode MODE       LowPower, Normal or Turbo (default: Normal)
```

## Requirements

Target S17 must allow SSH access.

import gleeunit
import gleeunit/should
import gleam/string
import gleam/option.{None, Some}
import systemd_status.{
  Active, Inactive, Loaded, OneshotService, Service, SimpleService,
}

pub fn main() {
  gleeunit.main()
}

pub const input_timer = "Unit=wibble-action-network-donations-sync.service
TimersMonotonic={ OnUnitInactiveUSec=3min ; next_elapse=5month 3w 20h 30min 38.992474s }
OnClockChange=no
OnTimezoneChange=no
NextElapseUSecMonotonic=5month 3w 20h 32min 49.712875s
LastTriggerUSec=Wed 2024-02-28 10:47:43 UTC
LastTriggerUSecMonotonic=5month 3w 20h 27min 36.611409s
Result=success
AccuracyUSec=5s
RandomizedDelayUSec=4min
FixedRandomDelay=no
Persistent=yes
WakeSystem=no
RemainAfterElapse=yes
Id=wibble-action-network-donations-sync.timer
Names=wibble-action-network-donations-sync.timer
Requires=-.mount wibble-action-network-donations-sync.service sysinit.target
WantedBy=wibble-action-network-donations-sync.service timers.target
Conflicts=shutdown.target
Before=timers.target wibble-action-network-donations-sync.service shutdown.target
After=-.mount sysinit.target
Triggers=wibble-action-network-donations-sync.service
RequiresMountsFor=/var/lib/systemd/timers
Description=Run wibble-action-network-donations-sync.service periodically
LoadState=loaded
ActiveState=active
FreezerState=running
SubState=waiting
FragmentPath=/lib/systemd/system/wibble-action-network-donations-sync.timer
UnitFileState=enabled
UnitFilePreset=enabled
StateChangeTimestamp=Wed 2024-02-28 10:47:46 UTC
StateChangeTimestampMonotonic=15037058992863
InactiveExitTimestamp=Tue 2023-09-26 10:46:38 UTC
InactiveExitTimestampMonotonic=1644991071302
ActiveEnterTimestamp=Tue 2023-09-26 10:46:38 UTC
ActiveEnterTimestampMonotonic=1644991071302
ActiveExitTimestamp=Mon 2023-09-25 16:23:55 UTC
ActiveExitTimestampMonotonic=1578828593399
InactiveEnterTimestamp=Mon 2023-09-25 16:23:55 UTC
InactiveEnterTimestampMonotonic=1578828593399
CanStart=yes
CanStop=yes
CanReload=no
CanIsolate=no
CanClean=state
CanFreeze=no
StopWhenUnneeded=no
RefuseManualStart=no
RefuseManualStop=no
AllowIsolate=no
DefaultDependencies=yes
OnSuccessJobMode=fail
OnFailureJobMode=replace
IgnoreOnIsolate=no
NeedDaemonReload=no
"

pub const input_service_simple = "Type=simple
Restart=on-failure
NotifyAccess=none
RestartUSec=100ms
TimeoutStartUSec=1min 30s
TimeoutStopUSec=1min 10s
TimeoutAbortUSec=1min 10s
TimeoutStartFailureMode=terminate
TimeoutStopFailureMode=terminate
RuntimeMaxUSec=infinity
WatchdogUSec=0
WatchdogTimestamp=n/a
WatchdogTimestampMonotonic=0
RootDirectoryStartOnly=no
RemainAfterExit=no
GuessMainPID=yes
MainPID=3764255
ControlPID=0
FileDescriptorStoreMax=0
NFileDescriptorStore=0
StatusErrno=0
Result=success
ReloadResult=success
CleanResult=success
UID=[not set]
GID=[not set]
NRestarts=0
OOMPolicy=stop
ExecMainStartTimestamp=Tue 2024-02-27 22:50:37 UTC
ExecMainStartTimestampMonotonic=14994030551365
ExecMainExitTimestamp=n/a
ExecMainExitTimestampMonotonic=0
ExecMainPID=3764255
ExecMainCode=0
ExecMainStatus=0
ExecStart={ path=deno ; argv[]=deno task production webapp ; ignore_errors=no ; start_time=[Tue 2024-02-27 22:50:37 UTC] ; stop_time=[n/a] ; pid=3764255 ; code=(null) ; status=0/0 }
ExecStartEx={ path=deno ; argv[]=deno task production webapp ; flags= ; start_time=[Tue 2024-02-27 22:50:37 UTC] ; stop_time=[n/a] ; pid=3764255 ; code=(null) ; status=0/0 }
Slice=system.slice
ControlGroup=/system.slice/wibble-webapp.service
MemoryCurrent=21995520
MemoryAvailable=infinity
CPUUsageNSec=1378112000
EffectiveCPUs=0
EffectiveMemoryNodes=0
TasksCurrent=4
IPIngressBytes=[no data]
IPIngressPackets=[no data]
IPEgressBytes=[no data]
IPEgressPackets=[no data]
IOReadBytes=18446744073709551615
IOReadOperations=18446744073709551615
IOWriteBytes=18446744073709551615
IOWriteOperations=18446744073709551615
Delegate=no
CPUAccounting=yes
CPUWeight=[not set]
StartupCPUWeight=[not set]
CPUShares=[not set]
StartupCPUShares=[not set]
CPUQuotaPerSecUSec=infinity
CPUQuotaPeriodUSec=infinity
IOAccounting=no
IOWeight=[not set]
StartupIOWeight=[not set]
BlockIOAccounting=no
BlockIOWeight=[not set]
StartupBlockIOWeight=[not set]
MemoryAccounting=yes
DefaultMemoryLow=0
DefaultMemoryMin=0
MemoryMin=0
MemoryLow=0
MemoryHigh=infinity
MemoryMax=infinity
MemorySwapMax=infinity
MemoryLimit=infinity
DevicePolicy=auto
TasksAccounting=yes
TasksMax=1013
IPAccounting=no
ManagedOOMSwap=auto
ManagedOOMMemoryPressure=auto
ManagedOOMMemoryPressureLimit=0
ManagedOOMPreference=none
Environment=PGHOST=localhost PGUSER=wibble PGPASSWORD=wibble
UMask=0022
LimitCPU=infinity
LimitCPUSoft=infinity
LimitFSIZE=infinity
LimitFSIZESoft=infinity
LimitDATA=infinity
LimitDATASoft=infinity
LimitSTACK=infinity
LimitSTACKSoft=8388608
LimitCORE=infinity
LimitCORESoft=0
LimitRSS=infinity
LimitRSSSoft=infinity
LimitNOFILE=524288
LimitNOFILESoft=1024
LimitAS=infinity
LimitASSoft=infinity
LimitNPROC=3377
LimitNPROCSoft=3377
LimitMEMLOCK=65536
LimitMEMLOCKSoft=65536
LimitLOCKS=infinity
LimitLOCKSSoft=infinity
LimitSIGPENDING=3377
LimitSIGPENDINGSoft=3377
LimitMSGQUEUE=819200
LimitMSGQUEUESoft=819200
LimitNICE=0
LimitNICESoft=0
LimitRTPRIO=0
LimitRTPRIOSoft=0
LimitRTTIME=infinity
LimitRTTIMESoft=infinity
WorkingDirectory=/usr/lib/wibble
OOMScoreAdjust=0
CoredumpFilter=0x33
Nice=0
IOSchedulingClass=2
IOSchedulingPriority=4
CPUSchedulingPolicy=0
CPUSchedulingPriority=0
CPUAffinityFromNUMA=no
NUMAPolicy=n/a
TimerSlackNSec=50000
CPUSchedulingResetOnFork=no
NonBlocking=no
StandardInput=null
StandardOutput=journal
StandardError=inherit
TTYReset=no
TTYVHangup=no
TTYVTDisallocate=no
SyslogPriority=30
SyslogLevelPrefix=yes
SyslogLevel=6
SyslogFacility=3
LogLevelMax=-1
LogRateLimitIntervalUSec=0
LogRateLimitBurst=0
SecureBits=0
CapabilityBoundingSet=cap_chown cap_dac_override cap_dac_read_search cap_fowner cap_fsetid cap_kill cap_setgid cap_setuid cap_setpcap cap_linux_immutable cap_net_bind_service cap_net_broadcast cap_net_admin cap_net_raw cap_ipc_lock cap_ipc_owner cap_sys_module cap_sys_rawio cap_sys_chroot cap_sys_ptrace cap_sys_pacct cap_sys_admin cap_sys_boot cap_sys_nice cap_sys_resource cap_sys_time cap_sys_tty_config cap_mknod cap_lease cap_audit_write cap_audit_control cap_setfcap cap_mac_override cap_mac_admin cap_syslog cap_wake_alarm cap_block_suspend cap_audit_read cap_perfmon cap_bpf cap_checkpoint_restore
DynamicUser=no
RemoveIPC=no
PrivateTmp=no
PrivateDevices=no
ProtectClock=no
ProtectKernelTunables=no
ProtectKernelModules=no
ProtectKernelLogs=no
ProtectControlGroups=no
PrivateNetwork=no
PrivateUsers=no
PrivateMounts=no
PrivateIPC=no
ProtectHome=no
ProtectSystem=no
SameProcessGroup=no
UtmpMode=init
IgnoreSIGPIPE=yes
NoNewPrivileges=no
SystemCallErrorNumber=2147483646
LockPersonality=no
RuntimeDirectoryPreserve=no
RuntimeDirectoryMode=0755
StateDirectoryMode=0755
CacheDirectoryMode=0755
LogsDirectoryMode=0755
ConfigurationDirectoryMode=0755
TimeoutCleanUSec=infinity
MemoryDenyWriteExecute=no
RestrictRealtime=no
RestrictSUIDSGID=no
RestrictNamespaces=no
MountAPIVFS=no
KeyringMode=private
ProtectProc=default
ProcSubset=all
ProtectHostname=no
KillMode=control-group
KillSignal=15
RestartKillSignal=15
FinalKillSignal=9
SendSIGKILL=yes
SendSIGHUP=no
WatchdogSignal=6
Id=wibble-webapp.service
Names=wibble-webapp.service
Requires=-.mount system.slice sysinit.target
Wants=network-online.target
WantedBy=graphical.target
Conflicts=shutdown.target
Before=shutdown.target graphical.target
After=basic.target network-online.target systemd-journald.socket sysinit.target system.slice -.mount
RequiresMountsFor=/usr/lib/wibble
Description=wibble webapp
LoadState=loaded
ActiveState=active
FreezerState=running
SubState=running
FragmentPath=/lib/systemd/system/wibble-webapp.service
UnitFileState=enabled
UnitFilePreset=enabled
StateChangeTimestamp=Tue 2024-02-27 22:50:37 UTC
StateChangeTimestampMonotonic=14994030551519
InactiveExitTimestamp=Tue 2024-02-27 22:50:37 UTC
InactiveExitTimestampMonotonic=14994030551519
ActiveEnterTimestamp=Tue 2024-02-27 22:50:37 UTC
ActiveEnterTimestampMonotonic=14994030551519
ActiveExitTimestamp=Tue 2024-02-27 22:50:37 UTC
ActiveExitTimestampMonotonic=14994030548078
InactiveEnterTimestamp=Tue 2024-02-27 22:50:37 UTC
InactiveEnterTimestampMonotonic=14994030550376
CanStart=yes
CanStop=yes
CanReload=no
CanIsolate=no
CanFreeze=yes
StopWhenUnneeded=no
RefuseManualStart=no
RefuseManualStop=no
AllowIsolate=no
DefaultDependencies=yes
OnSuccessJobMode=fail
OnFailureJobMode=replace
IgnoreOnIsolate=no
NeedDaemonReload=no
JobTimeoutUSec=infinity
JobRunningTimeoutUSec=infinity
JobTimeoutAction=none
ConditionResult=yes
AssertResult=yes
ConditionTimestamp=Tue 2024-02-27 22:50:37 UTC
ConditionTimestampMonotonic=14994030550692
AssertTimestamp=Tue 2024-02-27 22:50:37 UTC
AssertTimestampMonotonic=14994030550693
Transient=no
Perpetual=no
StartLimitIntervalUSec=10s
StartLimitBurst=5
StartLimitAction=none
FailureAction=none
SuccessAction=none
InvocationID=412291dd742048e0ab54c480a8f5446a
CollectMode=inactive
"

pub const input_service_oneshot = "Type=oneshot
Restart=no
NotifyAccess=none
RestartUSec=100ms
TimeoutStartUSec=infinity
TimeoutStopUSec=1min 30s
TimeoutAbortUSec=1min 30s
TimeoutStartFailureMode=terminate
TimeoutStopFailureMode=terminate
RuntimeMaxUSec=infinity
WatchdogUSec=0
WatchdogTimestamp=n/a
WatchdogTimestampMonotonic=0
RootDirectoryStartOnly=no
RemainAfterExit=no
GuessMainPID=yes
MainPID=0
ControlPID=0
FileDescriptorStoreMax=0
NFileDescriptorStore=0
StatusErrno=0
Result=success
ReloadResult=success
CleanResult=success
UID=[not set]
GID=[not set]
NRestarts=0
OOMPolicy=stop
ExecMainStartTimestamp=Wed 2024-02-28 12:00:42 UTC
ExecMainStartTimestampMonotonic=15041435362456
ExecMainExitTimestamp=Wed 2024-02-28 12:00:42 UTC
ExecMainExitTimestampMonotonic=15041435659467
ExecMainPID=3781234
ExecMainCode=1
ExecMainStatus=0
ExecStart={ path=/bin/sh ; argv[]=/bin/sh -c /usr/local/bin/wibble action-network-donations-sync ; ignore_errors=no ; start_time=[Wed 2024-02-28 12:00:42 UTC] ; stop_time=[Wed 2024-02-28 12:00:42 UTC] ; pid=3781234 ; code=exited ; status=0 }
ExecStartEx={ path=/bin/sh ; argv[]=/bin/sh -c /usr/local/bin/wibble action-network-donations-sync ; flags= ; start_time=[Wed 2024-02-28 12:00:42 UTC] ; stop_time=[Wed 2024-02-28 12:00:42 UTC] ; pid=3781234 ; code=exited ; status=0 }
Slice=system.slice
MemoryCurrent=[not set]
MemoryAvailable=infinity
CPUUsageNSec=165246000
TasksCurrent=[not set]
IPIngressBytes=[no data]
IPIngressPackets=[no data]
IPEgressBytes=[no data]
IPEgressPackets=[no data]
IOReadBytes=18446744073709551615
IOReadOperations=18446744073709551615
IOWriteBytes=18446744073709551615
IOWriteOperations=18446744073709551615
Delegate=no
CPUAccounting=yes
CPUWeight=[not set]
StartupCPUWeight=[not set]
CPUShares=[not set]
StartupCPUShares=[not set]
CPUQuotaPerSecUSec=infinity
CPUQuotaPeriodUSec=infinity
IOAccounting=no
IOWeight=[not set]
StartupIOWeight=[not set]
BlockIOAccounting=no
BlockIOWeight=[not set]
StartupBlockIOWeight=[not set]
MemoryAccounting=yes
DefaultMemoryLow=0
DefaultMemoryMin=0
MemoryMin=0
MemoryLow=0
MemoryHigh=infinity
MemoryMax=infinity
MemorySwapMax=infinity
MemoryLimit=infinity
DevicePolicy=auto
TasksAccounting=yes
TasksMax=1013
IPAccounting=no
ManagedOOMSwap=auto
ManagedOOMMemoryPressure=auto
ManagedOOMMemoryPressureLimit=0
ManagedOOMPreference=none
Environment=PGHOST=localhost PGUSER=wibble \"PGPASSWORD=one two\"
UMask=0022
LimitCPU=infinity
LimitCPUSoft=infinity
LimitFSIZE=infinity
LimitFSIZESoft=infinity
LimitDATA=infinity
LimitDATASoft=infinity
LimitSTACK=infinity
LimitSTACKSoft=8388608
LimitCORE=infinity
LimitCORESoft=0
LimitRSS=infinity
LimitRSSSoft=infinity
LimitNOFILE=524288
LimitNOFILESoft=1024
LimitAS=infinity
LimitASSoft=infinity
LimitNPROC=3377
LimitNPROCSoft=3377
LimitMEMLOCK=65536
LimitMEMLOCKSoft=65536
LimitLOCKS=infinity
LimitLOCKSSoft=infinity
LimitSIGPENDING=3377
LimitSIGPENDINGSoft=3377
LimitMSGQUEUE=819200
LimitMSGQUEUESoft=819200
LimitNICE=0
LimitNICESoft=0
LimitRTPRIO=0
LimitRTPRIOSoft=0
LimitRTTIME=infinity
LimitRTTIMESoft=infinity
OOMScoreAdjust=0
CoredumpFilter=0x33
Nice=0
IOSchedulingClass=2
IOSchedulingPriority=4
CPUSchedulingPolicy=0
CPUSchedulingPriority=0
CPUAffinityFromNUMA=no
NUMAPolicy=n/a
TimerSlackNSec=50000
CPUSchedulingResetOnFork=no
NonBlocking=no
StandardInput=null
StandardOutput=journal
StandardError=inherit
TTYReset=no
TTYVHangup=no
TTYVTDisallocate=no
SyslogPriority=30
SyslogLevelPrefix=yes
SyslogLevel=6
SyslogFacility=3
LogLevelMax=-1
LogRateLimitIntervalUSec=0
LogRateLimitBurst=0
SecureBits=0
CapabilityBoundingSet=cap_chown cap_dac_override cap_dac_read_search cap_fowner cap_fsetid cap_kill cap_setgid cap_setuid cap_setpcap cap_linux_immutable cap_net_bind_service cap_net_broadcast cap_net_admin cap_net_raw cap_ipc_lock cap_ipc_owner cap_sys_module cap_sys_rawio cap_sys_chroot cap_sys_ptrace cap_sys_pacct cap_sys_admin cap_sys_boot cap_sys_nice cap_sys_resource cap_sys_time cap_sys_tty_config cap_mknod cap_lease cap_audit_write cap_audit_control cap_setfcap cap_mac_override cap_mac_admin cap_syslog cap_wake_alarm cap_block_suspend cap_audit_read cap_perfmon cap_bpf cap_checkpoint_restore
DynamicUser=no
RemoveIPC=no
PrivateTmp=no
PrivateDevices=no
ProtectClock=no
ProtectKernelTunables=no
ProtectKernelModules=no
ProtectKernelLogs=no
ProtectControlGroups=no
PrivateNetwork=no
PrivateUsers=no
PrivateMounts=no
PrivateIPC=no
ProtectHome=no
ProtectSystem=no
SameProcessGroup=no
UtmpMode=init
IgnoreSIGPIPE=yes
NoNewPrivileges=no
SystemCallErrorNumber=2147483646
LockPersonality=no
RuntimeDirectoryPreserve=no
RuntimeDirectoryMode=0755
StateDirectoryMode=0755
CacheDirectoryMode=0755
LogsDirectoryMode=0755
ConfigurationDirectoryMode=0755
TimeoutCleanUSec=infinity
MemoryDenyWriteExecute=no
RestrictRealtime=no
RestrictSUIDSGID=no
RestrictNamespaces=no
MountAPIVFS=no
KeyringMode=private
ProtectProc=default
ProcSubset=all
ProtectHostname=no
KillMode=control-group
KillSignal=15
RestartKillSignal=15
FinalKillSignal=9
SendSIGKILL=yes
SendSIGHUP=no
WatchdogSignal=6
Id=wibble-action-network-donations-sync.service
Names=wibble-action-network-donations-sync.service
Requires=system.slice sysinit.target
Wants=wibble-action-network-donations-sync.timer
RequiredBy=wibble-action-network-donations-sync.timer
Conflicts=shutdown.target
Before=shutdown.target
After=basic.target systemd-journald.socket system.slice sysinit.target wibble-action-network-donations-sync.timer
TriggeredBy=wibble-action-network-donations-sync.timer
Description=wibble action-network-donations-sync
LoadState=loaded
ActiveState=inactive
FreezerState=running
SubState=dead
FragmentPath=/lib/systemd/system/wibble-action-network-donations-sync.service
UnitFileState=disabled
UnitFilePreset=enabled
StateChangeTimestamp=Wed 2024-02-28 12:00:42 UTC
StateChangeTimestampMonotonic=15041435659575
InactiveExitTimestamp=Wed 2024-02-28 12:00:42 UTC
InactiveExitTimestampMonotonic=15041435362623
ActiveEnterTimestamp=n/a
ActiveEnterTimestampMonotonic=0
ActiveExitTimestamp=n/a
ActiveExitTimestampMonotonic=0
InactiveEnterTimestamp=Wed 2024-02-28 12:00:42 UTC
InactiveEnterTimestampMonotonic=15041435659575
CanStart=yes
CanStop=yes
CanReload=no
CanIsolate=no
CanFreeze=yes
StopWhenUnneeded=no
RefuseManualStart=no
RefuseManualStop=no
AllowIsolate=no
DefaultDependencies=yes
OnSuccessJobMode=fail
OnFailureJobMode=replace
IgnoreOnIsolate=no
NeedDaemonReload=no
JobTimeoutUSec=infinity
JobRunningTimeoutUSec=infinity
JobTimeoutAction=none
ConditionResult=yes
AssertResult=yes
ConditionTimestamp=Wed 2024-02-28 12:00:42 UTC
ConditionTimestampMonotonic=15041435361567
AssertTimestamp=Wed 2024-02-28 12:00:42 UTC
AssertTimestampMonotonic=15041435361569
Transient=no
Perpetual=no
StartLimitIntervalUSec=10s
StartLimitBurst=5
StartLimitAction=none
FailureAction=none
SuccessAction=none
InvocationID=77746b9df2b84a84bc87b059ea0e28e3
CollectMode=inactive
"

pub fn decode_double_input_test() {
  { string.trim(input_service_oneshot) <> "\n\n" <> input_service_simple }
  |> systemd_status.parse_service
  |> should.be_ok
  |> should.equal(Service(
    id: "wibble-action-network-donations-sync.service",
    type_: OneshotService,
    load_state: Loaded,
    active_state: Inactive,
    sub_state: "dead",
    result: "success",
    description: Some("wibble action-network-donations-sync"),
    state_change_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
    active_enter_timestamp: None,
    active_exit_timestamp: None,
    inactive_enter_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
    inactive_exit_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
  ))
}

pub fn decode_service_oneshot_test() {
  input_service_oneshot
  |> systemd_status.parse_service
  |> should.be_ok
  |> should.equal(Service(
    id: "wibble-action-network-donations-sync.service",
    type_: OneshotService,
    load_state: Loaded,
    active_state: Inactive,
    sub_state: "dead",
    result: "success",
    description: Some("wibble action-network-donations-sync"),
    state_change_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
    active_enter_timestamp: None,
    active_exit_timestamp: None,
    inactive_enter_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
    inactive_exit_timestamp: Some("Wed 2024-02-28 12:00:42 UTC"),
  ))
}

pub fn decode_service_simple_test() {
  input_service_simple
  |> systemd_status.parse_service
  |> should.be_ok
  |> should.equal(Service(
    id: "wibble-webapp.service",
    type_: SimpleService,
    load_state: Loaded,
    active_state: Active,
    sub_state: "running",
    result: "success",
    description: Some("wibble webapp"),
    state_change_timestamp: Some("Tue 2024-02-27 22:50:37 UTC"),
    active_enter_timestamp: Some("Tue 2024-02-27 22:50:37 UTC"),
    active_exit_timestamp: Some("Tue 2024-02-27 22:50:37 UTC"),
    inactive_enter_timestamp: Some("Tue 2024-02-27 22:50:37 UTC"),
    inactive_exit_timestamp: Some("Tue 2024-02-27 22:50:37 UTC"),
  ))
}

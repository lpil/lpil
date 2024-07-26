import gleam/dict.{type Dict}
import gleam/dynamic.{type Dynamic}
import gleam/erlang/atom.{type Atom}
import gleam/erlang/process.{type Pid}
import gleam/list

pub type Strategy {
  /// If one child process terminates and is to be restarted, only that child
  /// process is affected. This is the default restart strategy.
  OneForOne

  /// If one child process terminates and is to be restarted, all other child
  /// processes are terminated and then all child processes are restarted.
  OneForAll

  /// If one child process terminates and is to be restarted, the 'rest' of the
  /// child processes (that is, the child processes after the terminated child
  /// process in the start order) are terminated. Then the terminated child
  /// process and all child processes after it are restarted.
  RestForOne
}

/// A supervisor can be configured to automatically shut itself down with exit
/// reason shutdown when significant children terminate with the auto_shutdown
/// key in the above map.
pub type AutoShutdown {
  /// Automic shutdown is disabled. This is the default setting.
  ///
  /// With auto_shutdown set to never, child specs with the significant flag
  /// set to true are considered invalid and will be rejected.
  Never
  /// The supervisor will shut itself down when any significant child
  /// terminates, that is, when a transient significant child terminates
  /// normally or when a temporary significant child terminates normally or
  /// abnormally.
  AnySignificant
  /// The supervisor will shut itself down when all significant children have
  /// terminated, that is, when the last active significant child terminates.
  /// The same rules as for any_significant apply.
  AllSignificant
}

// start_link() ->
//     supervisor:start_link({local, ?MODULE}, ?MODULE, []).

pub opaque type Builder {
  Builder(
    strategy: Strategy,
    intensity: Int,
    period: Int,
    auto_shutdown: AutoShutdown,
    children: List(ChildSpecification(Dynamic)),
  )
}

pub fn new(strategy strategy: Strategy) -> Builder {
  Builder(
    strategy: strategy,
    intensity: 1,
    period: 5,
    auto_shutdown: Never,
    children: [],
  )
}

/// To prevent a supervisor from getting into an infinite loop of child
/// process terminations and restarts, a maximum restart intensity is
/// defined using two integer values specified with keys intensity and
/// period in the above map. Assuming the values MaxR for intensity and MaxT
/// for period, then, if more than MaxR restarts occur within MaxT seconds,
/// the supervisor terminates all child processes and then itself. The
/// termination reason for the supervisor itself in that case will be
/// shutdown. 
///
/// Intensity defaults to 1 and period defaults to 5.
pub fn restart_tolerance(
  builder: Builder,
  intensity intensity: Int,
  period period: Int,
) -> Builder {
  Builder(..builder, intensity: intensity, period: period)
}

/// A supervisor can be configured to automatically shut itself down with
/// exit reason shutdown when significant children terminate.
pub fn auto_shutdown(builder: Builder, value: AutoShutdown) -> Builder {
  Builder(..builder, auto_shutdown: value)
}

/// Restart defines when a terminated child process must be restarted. 
pub type Restart {
  /// A permanent child process is always restarted.
  Permanent
  /// A transient child process is restarted only if it terminates abnormally,
  /// that is, with another exit reason than `normal`, `shutdown`, or
  /// `{shutdown,Term}`.
  Transient
  /// A temporary child process is never restarted (even when the supervisor's
  /// restart strategy is `RestForOne` or `OneForAll` and a sibling's death
  /// causes the temporary process to be terminated).
  Temporary
}

pub type ChildType {
  Worker(
    /// The number of milliseconds the child is given to shut down. The
    /// supervisor tells the child process to terminate by calling
    /// `exit(Child,shutdown)` and then wait for an exit signal with reason
    /// shutdown back from the child process. If no exit signal is received
    /// within the specified number of milliseconds, the child process is
    /// unconditionally terminated using `exit(Child,kill)`.
    shutdown_ms: Int,
  )
  Supervisor
}

pub type ChildSpecification(error) {
  ChildSpecification(
    /// id is used to identify the child specification internally by the
    /// supervisor.
    ///
    /// Notice that this identifier on occations has been called "name". As far
    /// as possible, the terms "identifier" or "id" are now used but to keep
    /// backward compatibility, some occurences of "name" can still be found, for
    /// example in error messages.
    id: String,
    /// A function to call to start the child process.
    start: fn() -> Result(Pid, error),
    /// When the child is to be restarted. See the `Restart` documentation for
    /// more.
    ///
    /// You most likely want the `Permanent` variant.
    restart: Restart,
    /// This defines if a child is considered significant for automatic
    /// self-shutdown of the supervisor.
    ///
    /// You most likely do not want to consider any children significant.
    ///
    /// This will be ignored if the supervisor auto shutdown is set to `Never`,
    /// which is the default.
    significant: Bool,
    /// Whether the child is a supervisor or not.
    child_type: ChildType,
  )
}

// sup_flags() = #{strategy => strategy(),           % optional
//                 intensity => non_neg_integer(),   % optional
//                 period => pos_integer(),          % optional
//                 auto_shutdown => auto_shutdown()} % optional
// init([]) ->
//     % Define the supervision strategy
//     RestartStrategy = rest_for_one,
//     MaxRestarts = 3,
//     MaxTime = 5,
//     SupFlags = {RestartStrategy, MaxRestarts, MaxTime},
//
//     % Define child specifications using maps
//     DbConnSpec = #{
//         id => db_connection,
//         start => {db_connection, start_link, []},
//         restart => permanent,
//         shutdown => 5000,
//         type => worker,
//         modules => [db_connection]
//     },
//
//     % List of child specifications
//     Children = [DbConnSpec],
//
//     {ok, {SupFlags, Children}}.

pub fn start_link(builder: Builder) -> Result(Pid, Dynamic) {
  let flags =
    dict.new()
    |> property("strategy", builder.strategy)
    |> property("intensity", builder.intensity)
    |> property("period", builder.period)
    |> property("auto_shutdown", builder.auto_shutdown)

  let children = builder.children |> list.reverse |> list.map(convert_child)

  atom.create_from_string("erlang_supervisor")
  |> erlang_start_link(#(flags, children))
}

fn convert_child(child: ChildSpecification(anything)) -> Dict(Atom, Dynamic) {
  let mfa = #(
    atom.create_from_string("erlang"),
    atom.create_from_string("apply"),
    [child.start],
  )

  let #(type_, shutdown) = case child.child_type {
    Supervisor -> #(
      atom.create_from_string("supervisor"),
      dynamic.from(atom.create_from_string("infinity")),
    )
    Worker(timeout) -> #(
      atom.create_from_string("worker"),
      dynamic.from(timeout),
    )
  }

  dict.new()
  |> property("id", child.id)
  |> property("start", mfa)
  |> property("restart", child.restart)
  |> property("type", type_)
  |> property("shutdown", shutdown)
}

fn property(
  dict: Dict(Atom, Dynamic),
  key: String,
  value: anything,
) -> Dict(Atom, Dynamic) {
  dict.insert(dict, atom.create_from_string(key), dynamic.from(value))
}

@external(erlang, "supervisor", "start_link")
fn erlang_start_link(
  module: Atom,
  args: #(Dict(Atom, Dynamic), List(Dict(Atom, Dynamic))),
) -> Result(Pid, Dynamic)

// Callback used by the Erlang supervisor module.
@internal
pub fn init(start_data: Dynamic) -> Result(Dynamic, never) {
  Ok(start_data)
}

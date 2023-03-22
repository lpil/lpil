type Result<T> = NotError<T> | Error;
type NotError<T> = Exclude<T, Error>;

// Run a function and return the result or any error thrown.
export function catching<T>(f: () => NotError<T>): Result<T> {
  try {
    return f();
  } catch (error) {
    return error;
  }
}

// Replace any error with a default value.
export function replaceError<A, B>(value: Result<A>, defaultValue: B): A | B {
  if (value instanceof Error) {
    return defaultValue;
  } else {
    return value;
  }
}

// Transform the success value of a result using a function.
export function mapResult<A, B>(
  value: Result<A>,
  f: (value: A) => NotError<B>
): Result<B> {
  if (value instanceof Error) {
    return value;
  } else {
    return f(value);
  }
}

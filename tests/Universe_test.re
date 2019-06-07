open Jest;
open Expect;

describe("Expect", () => {
  test("toBe", () =>
    expect(1 + 2) |> toBe(3)
  );

  test("toBe two", () =>
    expect(2 + 2) |> toBe(4)
  );
});

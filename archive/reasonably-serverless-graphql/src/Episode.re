type t = Js.t {. name : string, number : int};

/*
   Mock data
 */
let hope: t = {"name": "A New Hope", "number": 4};

let empire: t = {"name": "The Empire Strikes Back", "number": 5};

let jedi: t = {"name": "The Return of the Jedi", "number": 6};

/*
   Persistence functions
 */
let all () => [|hope, empire, jedi|];

let favourite () => empire;

let count () => Array.length (all ());

create TABLE person (
  id INTEGER PRIMARY KEY,
  first_name TEXT,
  last_name TEXT,
  age INTEGER
);

create TABLE pet (
  id INTEGER PRIMARY KEY,
  name TEXT,
  species TEXT,
  age INTEGER,
  dead INTEGER
);

create TABLE person_pet (
  person_id INTEGER,
  pet_id INTEGER
);

INSERT INTO pet (id, name, species, age, dead)
  VALUES (0, "Fluffy", "Unicorn", 1000, 0);

SELECT * FROM person;
SELECT * FROM pet;

UPDATE person SET first_name = "Guy"
  WHERE first_name = "Zed";

UPDATE pet SET name = "Fancy Pants"
  WHERE id = 0;

SELECT * FROM person;
SELECT * FROM pet;

/* Change it back to Zed */
UPDATE person SET first_name = "Zed"
  WHERE first_name = "Guy";

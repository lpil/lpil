/* run on db 2 */

SELECT * FROM person;

SELECT name, age FROM pet;

SELECT first_name, age FROM person WHERE first_name != "Zed";

SELECT name, age FROM pet WHERE age > 10;

SELECT name, age FROM pet WHERE age > 10 AND age < 2000;

SELECT name, age FROM pet
  WHERE age > 10
  AND age < 2000
  OR name = "Gigantor";

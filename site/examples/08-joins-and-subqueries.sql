CREATE TABLE author(id INTEGER PRIMARY KEY, name TEXT);
CREATE TABLE book(id INTEGER PRIMARY KEY, author INT, title TEXT, year INT);
INSERT INTO author VALUES (1,'Knuth'),(2,'Dijkstra'),(3,'Hopper');
INSERT INTO book VALUES (1,1,'TAOCP 1',1968),(2,1,'TeXbook',1984),(3,2,'A Discipline of Programming',1976);
SELECT a.name, count(b.id) AS books, min(b.year) AS first FROM author a LEFT JOIN book b ON b.author = a.id GROUP BY a.id ORDER BY books DESC, a.name;
SELECT name FROM author WHERE id NOT IN (SELECT author FROM book);
SELECT title, (SELECT name FROM author WHERE id = book.author) AS by FROM book WHERE year > (SELECT avg(year) FROM book);

CREATE TABLE posts
  ( id       integer primary key autoincrement
  , title    text    not null
  , contents text    not null
  , author   text    not null
  , time     integer not null
  , next     integer
  , prev     integer
  );

CREATE TABLE lookup
  ( id      integer primary key autoincrement
  , year    integer
  , month   integer
  , time    integer
  , slug    text
  , post_id integer references posts(id)
  );

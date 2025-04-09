create role deq_odbc;
grant select, insert, update, delete on all tables in schema public to deq_odbc;

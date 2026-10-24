SELECT typeof( unhex('') ), length( unhex('') );
SELECT unhex();
SELECT unhex('ABCD', '1234', '');
SELECT typeof( unhex(' ', ' -') ), length( unhex('-', ' -') );
SELECT hex( unhex('?ABCD?', '??') )
SELECT typeof( unhex('?ABCD?', '??') )
SELECT hex( unhex('?AB CD??', '? ?') )
SELECT typeof(unhex(NULL))
SELECT typeof(unhex(NULL, ' '))
SELECT typeof(unhex('1234', NULL))

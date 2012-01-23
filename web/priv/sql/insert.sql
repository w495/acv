/**
 * \file Вставка данных в базу
 *
**/

insert into permission_type (name) values ('static-test');

insert into permission_type (name)
	values
		('static'),
		('expert_conf'),
		('dir');

insert into permission (name, description, perm_type_id)
	values
		('admin', 'полный доступ',
			(select id from permission_type where name='static')),
		('w_conference', 'редактировать конференции',
			(select id from permission_type where name='static')),
 		('w_test', 'редактировать тесты',
			(select id from permission_type where name='static')),
		('w_user', 'редактировать пользователей',
			(select id from permission_type where name='static'));

insert into customer_group (name, description)
	values
		('admin', 	'администраторы'),
		('expert', 	'эксперты'),
		('all', 	'все пользователи');

insert into permission2group (perm_id, group_id)
	values
		( (select id from permission where name='admin'),
			(select id from customer_group where name='admin'));

insert into customer (firstname, lastname, patronimic, login, password_hash)
	values ('---', '---', '---', 'admin', '21232F297A57A5A743894A0E4A801FC3');

insert into customer2group (customer_id, group_id)
	values
		((select id from customer where login='admin'),
			(select id from customer_group where name='admin')),
		((select id from customer where login='admin'),
			(select id from customer_group where name='all'));



insert into banner_place (name, alias) values ('right', 'справа'), ('bottom', 'снизу');


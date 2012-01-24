/***********************************************************************
 *
 * \file Вставка данных в базу
 *
***********************************************************************/

insert into permission_type (name)
	values
		('static');

insert into permission (name, description, perm_type_id)
	values
		('admin', 'полный доступ',
			(select id from permission_type where name='static'));

insert into customer_group (name, description)
	values
		('admin', 	'администраторы');

insert into permission_2_group (perm_id, group_id)
	values
		( (select id from permission where name='admin'),
			(select id from customer_group where name='admin'));

insert into customer (firstname, lastname, patronimic, login, password_hash)
	values ('fadmin', 'ladmin', 'padmin',
        'admin', '21232F297A57A5A743894A0E4A801FC3');

    --//
    --     admin -> 21232F297A57A5A743894A0E4A801FC3
    --// новыйпароль tgv_region
    --     yjdsqgfhjkm -> C7BCC36975D86BB977D99A7DFB8EBDA0
    --//
    --     c7bcc36975d86bb977d99a7dfb8ebda0 -> D28847DA5504EE1365C159BC8FA18198
    --// md5sum этого файла
    --     ac4e05cfe177d66bf630dab627e81ab0 -> 344F01D45FFCE96499C7B8966BD176E0
    --//
    --     mjkqgjdsyhf -> C72DC633185E52C9FD363EEED7220C85
    --//
    --     etsuken -> C61B248A4D509E2923EBD983A8658C55


insert into customer_2_group (customer_id, group_id)
	values
		((select id from customer where login='admin'),
			(select id from customer_group where name='admin'));

insert into geo_region (alias, name) values ('world', 'весь мир');

insert into banner_place (name, alias)
    values
        ('right', 'справа'), ('bottom', 'снизу');


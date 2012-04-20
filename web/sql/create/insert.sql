/***********************************************************************
 *
 * \file Вставка данных в базу
 *
***********************************************************************/

insert into var (av_stats_max_id) values (0);


insert into permission_type (name)
	values
		('static');

insert into permission_entity_type (name)
    values
        ('customer');

insert into permission (name, description, perm_type_id, entity_type_id)
	values
		('admin', 'полный доступ',
			(select id from permission_type where name='static'),
            (select id from permission_entity_type where name='customer')
        ),
        ('undel_group', 'не удаляемые группы',
            (select id from permission_type where name='static'),
            (select id from permission_entity_type where name='customer')
        ),
        ('undel_customer', 'не удаляемые пользователи',
            (select id from permission_type where name='static'),
            (select id from permission_entity_type where name='customer')
        ),
        ('insider', 'доступ к личному кабинету',
            (select id from permission_type where name='static'),
            (select id from permission_entity_type where name='customer')
        ),
        ('sysmsg', 'право получать системные сообщения',
            (select id from permission_type where name='static'),
            (select id from permission_entity_type where name='customer')
        );

insert into customer_group (name, description)
	values
		('admin', 	         'администраторы'),
        ('sysmsg',           'получатели системных сообщений'),
        ('insider',          'пользователи');

insert into permission2group (perm_id, group_id)
	values
		( (select id from permission where name='admin'),
			(select id from customer_group where name='admin')),
        ( (select id from permission where name='undel_group'),
            (select id from customer_group where name='admin')),
        ( (select id from permission where name='undel_customer'),
            (select id from customer_group where name='admin')),
        ( (select id from permission where name='sysmsg'),
            (select id from customer_group where name='admin')),
        ( (select id from permission where name='insider'),
            (select id from customer_group where name='admin')),

        ( (select id from permission where name='undel_group'),
            (select id from customer_group where name='sysmsg')),
        ( (select id from permission where name='sysmsg'),
            (select id from customer_group where name='sysmsg')),
        ( (select id from permission where name='insider'),
            (select id from customer_group where name='sysmsg')),

        ( (select id from permission where name='undel_group'),
            (select id from customer_group where name='insider')),
        ( (select id from permission where name='insider'),
            (select id from customer_group where name='insider'));


insert into customer (firstname, lastname, patronimic,login, password_hash)
	values ('fadmin', 'ladmin', 'padmin',
        'admin', '21232F297A57A5A743894A0E4A801FC3');

    --//
    --     admin -> 21232F297A57A5A743894A0E4A801FC3
    --// новыйпароль
    --     yjdsqgfhjkm -> C7BCC36975D86BB977D99A7DFB8EBDA0
    --//
    --     c7bcc36975d86bb977d99a7dfb8ebda0 -> D28847DA5504EE1365C159BC8FA18198
    --// md5sum этого файла
    --     ac4e05cfe177d66bf630dab627e81ab0 -> 344F01D45FFCE96499C7B8966BD176E0
    --//
    --     mjkqgjdsyhf -> C72DC633185E52C9FD363EEED7220C85
    --//
    --     etsuken -> C61B248A4D509E2923EBD983A8658C55


insert into customer2group (customer_id, group_id)
	values
		((select id from customer where login='admin'),
			(select id from customer_group where name='admin')),
        ((select id from customer where login='admin'),
            (select id from customer_group where name='sysmsg')),
        ((select id from customer where login='admin'),
            (select id from customer_group where name='insider'));

insert into geo_area (name_ru, name_en) values ('СНГ', 'SNG');

insert into config (acv_video_loadnext) values (10);


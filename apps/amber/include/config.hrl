-ifndef(CONFIG_H).
-define(CONFIG_H, true).


-record(supervised_node, {
	mod_id  :: {module(), tuple()},
	conf    :: any()
}).

-record(supervised_driver, {
	mod_id  :: {module(), tuple()},
	dev_ti  :: {non_neg_integer(), non_neg_integer()},
	conf    :: any()
}).



-endif.
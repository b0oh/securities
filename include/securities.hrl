-record(op, {timestamp,
             price,
             amount}).

-record(entry, {start_timestamp,
                start_price,
                end_price,
                min_price,
                max_price,
                amount}).

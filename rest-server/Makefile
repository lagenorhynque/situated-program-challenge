HOST = localhost
PORT = 5433
USER = meetup
PASSWORD = password123
DB = meetup_test

create-tables:
	@export PGPASSWORD=$(PASSWORD) && \
	make up-scripts | xargs -n 1 psql -h $(HOST) -p $(PORT) -U $(USER) -d $(DB) -f

drop-tables:
	@export PGPASSWORD=$(PASSWORD) && \
	make down-scripts | xargs -n 1 psql -h $(HOST) -p $(PORT) -U $(USER) -d $(DB) -f

up-scripts:
	@find ../resources/migrations -name *.up.sql | sort

down-scripts:
	@find ../resources/migrations -name *.down.sql | sort | gtac

import datetime
from datetime import timezone
import json

latest = {
    "date" : f"{datetime.datetime.now(tz=timezone.utc):%Y-%m-%d %H:%M:%S}"
}

with open("./last_updated.json", mode='w') as file:
    json.dump(latest, file)

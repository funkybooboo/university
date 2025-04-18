import random
import time


update_types = [
    "created",
    "shipped",
    "location",
    "delivered",
    "delayed",
    "lost",
    "canceled",
    "noteadded"
]


def generate_random_update(update_type):
    shipment_id = f"s{random.randint(10000, 99999)}"  # Random shipment ID (format sXXXXX)
    timestamp = int(time.time()) - random.randint(3600, 2592000)  # Random timestamp within last month (in seconds)

    if update_type == "created":
        return f"{update_type},{shipment_id},{timestamp}"

    elif update_type == "shipped":
        expected_delivery_timestamp = timestamp + random.randint(86400, 604800)  # 1 to 7 days in future
        return f"{update_type},{shipment_id},{timestamp},{expected_delivery_timestamp}"

    elif update_type == "location":
        locations = ["Los Angeles CA", "New York NY", "Chicago IL", "Houston TX", "Phoenix AZ"]
        location = random.choice(locations)
        return f"{update_type},{shipment_id},{timestamp},{location}"

    elif update_type == "delivered":
        return f"{update_type},{shipment_id},{timestamp}"

    elif update_type == "delayed":
        new_expected_delivery_timestamp = timestamp + random.randint(86400, 604800)  # 1 to 7 days in future
        return f"{update_type},{shipment_id},{timestamp},{new_expected_delivery_timestamp}"

    elif update_type == "lost" or update_type == "canceled":
        return f"{update_type},{shipment_id},{timestamp}"

    elif update_type == "noteadded":
        notes = [
            "package was damaged slightly during shipping",
            "shipping label reprinted because of damage",
            "inspection completed on exported goods"
        ]
        note = random.choice(notes)
        return f"{update_type},{shipment_id},{timestamp},{note}"


def main():
    num_updates = 5000
    with open("shipments.txt", "w") as file:
        for _ in range(num_updates):
            update_type = random.choice(update_types)
            update_data = generate_random_update(update_type)
            file.write(f"{update_data}\n")


if __name__ == "__main__":
    main()

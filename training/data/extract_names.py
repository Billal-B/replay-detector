import os


def get_names(ds_name):
    for label in os.listdir(ds_name):
        for example in os.listdir(os.path.join(ds_name, label)):
            print(ds_name + "," + label + "," + example + ",20")


get_names("train")
get_names("test")

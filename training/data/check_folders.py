import os


ds_type = 'train'
ds_names = ['unk', 'not_logo']
for ds_name in ds_names:
    for example_folder in os.listdir(os.path.join(ds_type, ds_name)):
        if len(os.listdir(os.path.join(ds_type, ds_name, example_folder))) != 19:
            print(ds_name + " " + example_folder)

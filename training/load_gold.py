import os

def load_gold(gold_file):
    logos = []
    not_logos = []
    f_logo = open("gold/" + gold_file + "_logo")
    l_logo = f_logo.readline()
    while l_logo:
        logos.append(l_logo.strip())
        l_logo = f_logo.readline()
    f_nlogo = open("gold/" + gold_file + "_non_logo")
    l_nlogo = f_nlogo.readline()
    while l_nlogo:
        not_logos.append(l_nlogo.strip())
        l_nlogo = f_nlogo.readline()
    return (set(logos), set(not_logos))

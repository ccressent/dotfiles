import re
import commands

mapping = { 'INBOX':              'inbox'
          , '[Gmail]/All Mail':   'archive'
          , '[Gmail]/Drafts':     'drafts'
          , '[Gmail]/Sent Mail':  'sent'
          , '[Gmail]/Spam':       'spam'
          , '[Gmail]/Starred':    'starred'
          , '[Gmail]/Trash':      'trash'
          }

r_mapping = { val: key for val, key in mapping.items() }


def nametrans_local(folder):
    try:
        return r_mapping[folder]
    except:
        return re.sub('_', ' ', folder).capitalize()


def nametrans_remote(folder):
    try:
        return mapping[folder]
    except:
        return re.sub(' ', '_', folder).lower()


'''
Retrieve a password from OSX's keychain
'''

def get_keychain_passwd(account=None, server=None):
    params = {
        'security': '/usr/bin/security',
        'command': 'find-internet-password',
        'account': account,
        'server': server
    }
    command = "%(security)s %(command)s -w -a %(account)s -s %(server)s" % params
    return commands.getoutput(command)


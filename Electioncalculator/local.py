from WebApp.settings.base import *

DATABASES = {
    'default': {
        'ENGINE': 'django.db.backends.mysql',
        'NAME': 'tpc_election_calculator',
        'USER': 'root',
        'PASSWORD': 'root',
        'HOST': 'election-mysql',
        'PORT': 3306
    }
}

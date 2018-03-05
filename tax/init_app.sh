__init_app() {

        rm -rf WebApp/taxcalc/static/taxcalc/taxcalc/
        echo "Create static files."
        python manage.py collectstatic --noinput
        echo "Make migration."
        python manage.py makemigrations
        echo "Create migration."
        python manage.py migrate
        echo "Finished python initializations"

                echo "-------------------------------------------------------------------"
                echo "Create Superuser"
                script="
from django.contrib.auth.models import User;

username = '$ADMIN_USER';
password = '$ADMIN_PASSWORD';
email = '$ADMIN_EMAIL';

if User.objects.filter(username=username).count()==0:
    User.objects.create_superuser(username, email, password);
    print('Superuser created.');
else:
    print('Superuser creation skipped.');
        "
        echo $script

        printf "$script" | python manage.py shell
        echo "-------------------------------------------------------------------"


}

# Call all functions
__init_app

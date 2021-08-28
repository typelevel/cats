function addSponsor(divId, member, size) {
    div = document.getElementById(divId);
    var a = document.createElement('a');
    a.setAttribute('href', member.website || member.profile || '#');
    var img = document.createElement('img');
    img.setAttribute('src', member.image || 'img/missing-avatar.svg');
    if (size) {
        img.setAttribute('height', size);
        img.setAttribute('width', size);
    }
    img.setAttribute('alt', member.name);
    img.setAttribute('style', 'margin:6px;');
    if (member.marginBottom)
        img.setAttribute('style', img.getAttribute('style') + 'margin-bottom:' + member.marginBottom + 'px;')
    a.appendChild(img);
    div.appendChild(a);
};

const PlatinumSize = 98;
const GoldSize = 76;
const SilverSize = 60;
const BackerSize = 45;
const ContributorSize = 36;

var sponsors = function() {
    var xhr = new XMLHttpRequest();
    xhr.open('GET', 'https://opencollective.com/typelevel/members/all.json', true);
    xhr.responseType = 'json';
    xhr.onload = function() {
        var status = xhr.status;
        if (status === 200) {
            for(i = 0; i < xhr.response.length; i++) {
                var member = xhr.response[i];
                if (member.isActive) {
                    switch (member.tier) {
                    case 'Platinum Sponsor':
                        addSponsor('platinum-sponsors', member, PlatinumSize);
                    case 'Gold Sponsor':
                        addSponsor('gold-sponsors', member, GoldSize);
                    case 'Silver Sponsor':
                        addSponsor('silver-sponsors', member, SilverSize);
                    case 'backer':
                        addSponsor('backers', member, BackerSize);
                        break;
                    default:
                        if (member.totalAmountDonated > 0) {
                            addSponsor('other-contributors', member, ContributorSize);
                        }
                    }
                };
            }
        }
    };
    xhr.send();
};
sponsors();
// Add sponsors who predate open collective
addSponsor('gold-sponsors', {
    name: "47 Degrees",
    website: "https://47deg.com",
    image: "https://typelevel.org/cats/img/sponsors/47_degree.png"
});
addSponsor('gold-sponsors', {
    name: "Iterators",
    website: "https://iteratorshq.com",
    image: "https://typelevel.org/cats/img/sponsors/iterators.png",
    marginBottom: 20
});
addSponsor('gold-sponsors', {
    name: "Triplequote",
    website: "https://triplequote.com",
    image: "https://typelevel.org/cats/img/sponsors/triplequote.png",
    marginBottom: 20
});
addSponsor('gold-sponsors', {
    name: "Underscore",
    website: "https://underscore.com",
    image: "https://typelevel.org/cats/img/sponsors/underscore.png",
    marginBottom: 10
});
addSponsor('silver-sponsors', {
    name: "Ebiznext",
    website: "https://ebiznext.com",
    image: "https://typelevel.org/cats/img/sponsors/ebiznext.png",
    marginBottom: 10
});
addSponsor('silver-sponsors', {
    name: "Inner Product",
    website: "https://inner-product.com",
    image: "https://typelevel.org/cats/img/sponsors/inner-product.png"
});
addSponsor('silver-sponsors', {
    name: "Evolution Gaming Engineering",
    website: "https://evolutiongaming.com",
    image: "https://typelevel.org/cats/img/sponsors/evolution_gaming_engineering.png"
});

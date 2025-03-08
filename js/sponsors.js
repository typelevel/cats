function addSponsor(divId, member, size) {
    div = document.getElementById(divId);
    var a = document.createElement('a');
    a.setAttribute('href', member.website || (member.slug ? "https://opencollective.com/" + member.slug : null) || '#');
    var img = document.createElement('img');
    img.setAttribute('src', member.imageUrl || 'img/missing-avatar.svg');
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

var compareSponsors = function(a, b) {
    const levelDiff = levelToInt(a) - levelToInt(b);
    if (levelDiff === 0) {
        return a.account.name.localeCompare(b.account.name);
    }
    return levelDiff;
}

var levelToInt = function(member) {
    switch (member.tier ? member.tier.name : null) {
        case 'Platinum Sponsor':  
            return 1;
        case 'Gold Sponsor':
            return 2;
        case 'Silver Sponsor':
            return 3;
        case 'backer':
            return 4;
        default:
            if (member.totalDonations.valueInCents > 0) {
                return 5;
            }
            return 6;
    };
}

var sponsors = async function () {
    var response = await fetch('https://api.opencollective.com/graphql/v2', {
        method: 'POST',
        headers: {
            'Accept': 'application/json',
            'Content-Type': 'application/json'
        },
        body: JSON.stringify({ query: "query{collective(slug:\"typelevel\"){members{nodes{account{name slug website imageUrl isActive}tier{name}totalDonations{valueInCents}}}}}" })
    });

    if (response.ok) {
        var json = await response.json();
        var members = json.data.collective.members.nodes.sort(compareSponsors);
        const addedSponsors = {};

        for (i = 0; i < members.length; i++) {
            var member = members[i];
            if (addedSponsors[member.account.slug]) {
              continue;
            }

            addedSponsors[member.account.slug] = true;
            switch (member.tier ? member.tier.name : null) {
                case 'Platinum Sponsor':
                    addSponsor('platinum-sponsors', member.account, PlatinumSize);
                    break;
                case 'Gold Sponsor':
                    addSponsor('gold-sponsors', member.account, GoldSize);
                    break;
                case 'Silver Sponsor':
                    addSponsor('silver-sponsors', member.account, SilverSize);
                    break;
                case 'backer':
                    addSponsor('backers', member.account, BackerSize);
                    break;
                default:
                    if (member.totalDonations.valueInCents > 0) {
                        addSponsor('other-contributors', member.account, ContributorSize);
                    }
            };
        }
    }
};
sponsors();
// Add sponsors who predate open collective
addSponsor('gold-sponsors', {
    name: "47 Degrees",
    website: "https://47deg.com",
    imageUrl: "img/sponsors/47_degree.png"
});
addSponsor('gold-sponsors', {
    name: "Iterators",
    website: "https://iteratorshq.com",
    imageUrl: "img/sponsors/iterators.png",
    marginBottom: 20
});
addSponsor('gold-sponsors', {
    name: "Triplequote",
    website: "https://triplequote.com",
    imageUrl: "img/sponsors/triplequote.png",
    marginBottom: 20
});
addSponsor('gold-sponsors', {
    name: "Underscore",
    website: "https://underscore.com",
    imageUrl: "img/sponsors/underscore.png",
    marginBottom: 10
});
addSponsor('silver-sponsors', {
    name: "Ebiznext",
    website: "https://ebiznext.com",
    imageUrl: "img/sponsors/ebiznext.png",
    marginBottom: 10
});
addSponsor('silver-sponsors', {
    name: "Inner Product",
    website: "https://inner-product.com",
    imageUrl: "img/sponsors/inner-product.png"
});
addSponsor('silver-sponsors', {
    name: "Evolution Gaming Engineering",
    website: "https://evolutiongaming.com",
    imageUrl: "img/sponsors/evolution_gaming_engineering.png"
});
